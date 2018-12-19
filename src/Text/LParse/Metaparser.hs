module Text.LParse.Metaparser (specParse,metaParser,AST) where

import Control.Applicative
import Control.Arrow

import Data.Char
import Data.List
import qualified Data.Map.Strict as M

import Text.LParse.Parser
import Text.LParse.Prebuilt

data Token = Literal Char | CharClass String | Integer | Digit | Word | Star | Plus | May | Or | Is | Eoi | LParen | RParen | Sep | Whitespace deriving (Show,Eq)
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

metaTokenizer :: Parser r String [Token]
metaTokenizer = many (
    (consumeSingle '\\' >> escaped)
    <|> surround "[]" charclass
    <|> simpleSpecial
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

atomParser :: Parser r [Token] (Parser r' String AST)
atomParser = iLeafParser
    <|> dLeafParser
    <|> sLeafParser
    <|> eoiParser
    <|> charClassParser
    <|> charParser

starFreeParser :: Parser r [Token] (Parser r' String AST)
starFreeParser = surround [LParen,RParen] cfexParser <|> atomParser

concatFreeParser :: Parser r [Token] (Parser r' String AST) 
concatFreeParser = do
    sf <- starFreeParser
    (consumeSingle Star >> return (Node "many" <$> many sf)) 
        <|> (consumeSingle Plus >> return (Node "some" <$> some sf)) 
        <|> return sf

orFreeParser :: Parser r [Token] (Parser r' String AST)
orFreeParser = (\ps -> if length ps == 1 then head ps else Node "concat" <$> sequenceA ps) <$> some concatFreeParser 

cfexParser :: Parser r [Token] (Parser r' String AST)
cfexParser = (\ps -> if length ps == 1 then head ps else Node "or" <$> sequenceA ps) <$> sepSome (consumeSingle Or) orFreeParser

ruleParser :: Parser r [Token] (String,Parser r' String AST)
ruleParser = (,) <$> (many (nParse isLiteral (tokenParse getLiteral) "Expected Literal") << consumeSingle Is) <*> cfexParser

rulesetParser :: Parser r [Token] (M.Map String (Parser r' String AST))
rulesetParser = M.fromList<$> sepMany (consumeSingle Sep) ruleParser

combine :: M.Map String (Parser r' String AST) -> Parser r' String AST -> Parser r' String AST
combine rs e = e

fullParser :: Parser r [Token] (Parser r' String AST)
fullParser = combine <$> rulesetParser <*> cfexParser

parserParser :: Parser r [Token] (Parser r' String AST)
parserParser = cfexParser << eoi

metaParser :: Parser r String (Parser r' String AST)
metaParser = metaTokenizer >>> skip [Whitespace] parserParser

specParse :: String -> String -> Either String AST
specParse g i = doParse metaParser g >>= (`doParse` i)