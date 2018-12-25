module Text.LParse.Metaparser (specParse,metaParser,AST) where

import Control.Applicative
import Control.Arrow
import Control.Monad.Fix

import Data.Char
import Data.List
import qualified Data.Map.Strict as M

import Text.LParse.Parser
import Text.LParse.Prebuilt

data Token = Literal Char | CharClass String | RuleName String | Integer | Digit | Word | Star | Plus | May | Or | Is | Eoi | LParen | RParen | Sep | Whitespace deriving (Show,Eq)
data AST = Node String [AST] | ILeaf Integer | SLeaf String | EOI deriving Eq

instance Show AST where
    show (Node s cs) = s ++ "(" ++ intercalate "," (map show cs) ++ ")"
    show (ILeaf i) = show i
    show (SLeaf s) = s
    show EOI = "$"

escaped :: Parser r String Token
escaped = consumeSReturn 'i' Integer
    <|> consumeSReturn 'd' Digit
    <|> consumeSReturn 'w' Word
    <|> consumeSReturn '\\' (Literal '\\')
    <|> consumeSReturn '*' (Literal '*')
    <|> consumeSReturn '+' (Literal '+')
    <|> consumeSReturn '?' (Literal '?')
    <|> consumeSReturn '[' (Literal '[')
    <|> consumeSReturn ']' (Literal ']')
    <|> consumeSReturn '(' (Literal '(')
    <|> consumeSReturn ')' (Literal ')')
    <|> consumeSReturn '$' (Literal '$')

charclass :: Parser r String Token
charclass = CharClass <$> some (nParse (/=']') tokenReturn "Expected character")

simpleSpecial :: Parser r String Token
simpleSpecial = consumeSReturn '*' Star
    <|> consumeSReturn '+' Plus
    <|> consumeSReturn '?' May
    <|> consumeSReturn '|' Or
    <|> consumeSReturn '$' Eoi
    <|> consumeSReturn '(' LParen
    <|> consumeSReturn ')' RParen
    <|> consumeReturn "::=" Is
    <|> consumeSReturn ';' Sep

whitespace :: Parser r String Token
whitespace = some (nParse isSpace (return ()) "Expected Space") >> return Whitespace

ruleName :: Parser r String Token
ruleName = consumeSingle '%' >> (RuleName <$> word)

metaTokenizer :: Parser r String [Token]
metaTokenizer = many (
    (consumeSingle '\\' >> escaped)
    <|> surround "[]" charclass
    <|> simpleSpecial
    <|> ruleName
    <|> whitespace
    <|> (Literal <$> tokenReturn)
    )

iLeafParser :: Parser r [Token] (Parser r' String AST)
iLeafParser = consumeSReturn Integer (ILeaf <$> integer)

dLeafParser :: Parser r [Token] (Parser r' String AST)
dLeafParser = consumeSReturn Digit (ILeaf <$> digit)

sLeafParser :: Parser r [Token] (Parser r' String AST)
sLeafParser = consumeSReturn Word (SLeaf <$> word)

eoiParser :: Parser r [Token] (Parser r' String AST)
eoiParser = consumeSReturn Eoi (eoi >> return EOI)

isRuleName :: Token -> Bool
isRuleName (RuleName _) = True
isRuleName _ = False

getRuleName :: Token -> String
getRuleName (RuleName s) = s

subRuleParser :: M.Map String (Parser r' String AST) -> Parser r [Token] (Parser r' String AST)
subRuleParser m = nParse isRuleName (tokenParse ((m M.!) . getRuleName)) "Expected RuleName"

charClassCharParser :: String -> Parser r String Char
charClassCharParser (c:s) | c == '^' = nParse (not . (`elem` s)) tokenReturn ("Expected not [" ++ s ++ "]")
charClassCharParser s = nParse (`elem` s) tokenReturn ("Expected [" ++ s ++ "]")

isCharClass :: Token -> Bool
isCharClass (CharClass _) = True
isCharClass _ = False

getCharClass :: Token -> String
getCharClass (CharClass s) = s

charClassParser :: Parser r [Token] (Parser r' String AST)
charClassParser = nParse isCharClass (tokenParse (fmap (SLeaf . return) . charClassCharParser . getCharClass)) "Expected character class"

isLiteral :: Token -> Bool
isLiteral (Literal _) = True
isLiteral _ = False

getLiteral :: Token -> Char
getLiteral (Literal s) = s

parseLiteral :: Parser r [Token] Char
parseLiteral = nParse isLiteral (tokenParse getLiteral) "Expected Literal"

charParser :: Parser r [Token] (Parser r' String AST)
charParser = fmap (SLeaf . return) . (\c -> consumeSReturn c c) <$> parseLiteral

atomParser :: M.Map String (Parser r' String AST) -> Parser r [Token] (Parser r' String AST)
atomParser m = iLeafParser
    <|> dLeafParser
    <|> sLeafParser
    <|> eoiParser
    <|> charClassParser
    <|> subRuleParser m
    <|> charParser

starFreeParser :: M.Map String (Parser r' String AST) -> Parser r [Token] (Parser r' String AST)
starFreeParser m = surround [LParen,RParen] (cfexParser m) <|> atomParser m

concatFreeParser :: M.Map String (Parser r' String AST) -> Parser r [Token] (Parser r' String AST) 
concatFreeParser m = do
    sf <- starFreeParser m
    (consumeSingle Star >> return (Node "many" <$> many sf)) 
        <|> (consumeSingle Plus >> return (Node "some" <$> some sf)) 
        <|> return sf

orFreeParser :: M.Map String (Parser r' String AST) -> Parser r [Token] (Parser r' String AST)
orFreeParser m = (\ps -> if length ps == 1 then head ps else Node "concat" <$> sequenceA ps) <$> some (concatFreeParser m)

cfexParser :: M.Map String (Parser r' String AST) -> Parser r [Token] (Parser r' String AST)
cfexParser m = foldl1 (<|>) <$> sepSome (consumeSingle Or) (orFreeParser m)

ruleParser :: M.Map String (Parser r' String AST) -> Parser r [Token] (String,Parser r' String AST)
ruleParser m = (,) <$> (many (nParse isLiteral (tokenParse getLiteral) "Expected Literal") << consumeSingle Is) <*> cfexParser m

rulesetParser :: M.Map String (Parser r' String AST) -> Parser r [Token] (M.Map String (Parser r' String AST))
rulesetParser m = M.fromList<$> sepMany (consumeSingle Sep) (ruleParser m)

rulesetLoop :: Parser r [Token] (M.Map String (Parser r' String AST))
rulesetLoop = pfix rulesetParser

combine :: Maybe (M.Map String (Parser r' String AST)) -> Parser r [Token] (Parser r' String AST)
combine Nothing = cfexParser M.empty
combine (Just rs) = cfexParser rs 

fullParser :: Parser r [Token] (Parser r' String AST)
fullParser = try (rulesetLoop << consumeSingle Sep) >>= combine

parserParser :: Parser r [Token] (Parser r' String AST)
parserParser = fullParser << eoi

metaParser :: Parser r String (Parser r' String AST)
metaParser = metaTokenizer >>> skip [Whitespace] parserParser

specParse :: String -> String -> Either String AST
specParse g i = doParse metaParser g >>= (`doParse` i)