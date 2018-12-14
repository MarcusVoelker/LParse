module Text.LParse.Metaparser (specParse,metaParser,AST) where

import Control.Applicative
import Control.Arrow

import Text.LParse.Atomics
import Text.LParse.Parser
import Text.LParse.Transformers

data Token = Literal Char | CharClass String | Special Char | Star | Plus | May | Eoi | LParen | RParen deriving (Show,Eq)
data AST = Node String [AST] | ILeaf Integer | SLeaf String | EOI deriving Show

escaped :: Parser r String Token
escaped = consumeReturn 'i' (Special 'i')
    <|> consumeReturn 'd' (Special 'd')
    <|> consumeReturn 'w' (Special 'w')
    <|> consumeReturn '\\' (Literal '\\')
    <|> consumeReturn '*' (Literal '*')
    <|> consumeReturn '+' (Literal '+')
    <|> consumeReturn '?' (Literal '?')
    <|> consumeReturn '[' (Literal '[')
    <|> consumeReturn ']' (Literal ']')
    <|> consumeReturn '(' (Literal '(')
    <|> consumeReturn ')' (Literal ')')
    <|> consumeReturn '$' (Literal '$')

charclass :: Parser r String Token
charclass = CharClass <$> some (nParse (/=']') tokenReturn "Expected character")

simpleSpecial :: Parser r String Token
simpleSpecial = consumeReturn '*' Star
    <|> consumeReturn '+' Plus
    <|> consumeReturn '?' May
    <|> consumeReturn '$' Eoi
    <|> consumeReturn '(' LParen
    <|> consumeReturn ')' RParen

metaTokenizer :: Parser r String [Token]
metaTokenizer = many (
    (consumeSingle '\\' >> escaped)
    <|> surround "[]" charclass
    <|> simpleSpecial
    <|> (Literal <$> tokenReturn)
    )

iLeafParser :: Parser r [Token] (Parser r' String AST)
iLeafParser = consumeSingle (Special 'i') >> return (ILeaf <$> integer)

dLeafParser :: Parser r [Token] (Parser r' String AST)
dLeafParser = consumeSingle (Special 'd') >> return (ILeaf <$> digit)

sLeafParser :: Parser r [Token] (Parser r' String AST)
sLeafParser = consumeSingle (Special 'w') >> return (SLeaf <$> word)

eoiParser :: Parser r [Token] (Parser r' String AST)
eoiParser = consumeSingle Eoi >> return (eoi >> return EOI)

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

atomParser :: Parser r [Token] (Parser r' String AST)
atomParser = iLeafParser
    <|> dLeafParser
    <|> sLeafParser
    <|> eoiParser
    <|> charClassParser

starFreeParser :: Parser r [Token] (Parser r' String AST)
starFreeParser = surround [LParen,RParen] cfexParser <|> atomParser

concatFreeParser :: Parser r [Token] (Parser r' String AST) 
concatFreeParser = do
    sf <- starFreeParser
    (consumeSingle Star >> return (Node "many" <$> many sf)) 
        <|> (consumeSingle Plus >> return (Node "some" <$> some sf)) 
        <|> return sf


cfexParser :: Parser r [Token] (Parser r' String AST)
cfexParser = (\ps -> Node "concat" <$> sequenceA ps) <$> some concatFreeParser

parserParser :: Parser r [Token] (Parser r' String AST)
parserParser = cfexParser << eoi

metaParser :: Parser r String (Parser r' String AST)
metaParser = metaTokenizer >>> parserParser

specParse :: String -> String -> Either String AST
specParse g i = doParse metaParser g >>= (`doParse` i)