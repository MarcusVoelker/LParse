module Text.LParse.Description (specParse,parserParser,AST) where

import Control.Applicative

import Text.LParse.Atomics
import Text.LParse.Parser
import Text.LParse.Transformers

data AST = Node String [AST] | ILeaf Integer | SLeaf String | EOI deriving Show

iLeafParser :: Parser r String (Parser r' String AST)
iLeafParser = consume "\\i" >> return (ILeaf <$> integer)

dLeafParser :: Parser r String (Parser r' String AST)
dLeafParser = consume "\\d" >> return (ILeaf <$> digit)

sLeafParser :: Parser r String (Parser r' String AST)
sLeafParser = consume "\\w" >> return (SLeaf <$> word)

eoiParser :: Parser r String (Parser r' String AST)
eoiParser = consume "\\e" >> return (eoi >> return EOI)

charClassCharParser :: String -> Parser r String Char
charClassCharParser (c:s) | c == '^' = nParse (not . (`elem` s)) tokenReturn ("Expected not [" ++ s ++ "]")
charClassCharParser s = (nParse (`elem` s) tokenReturn ("Expected [" ++ s ++ "]"))

charClassParser :: Parser r String (Parser r' String AST)
charClassParser = do
    consumeSingle '['
    content <- some (nParse (/=']') tokenReturn "Expected character")
    consumeSingle ']'
    return (SLeaf . return <$> charClassCharParser content)

atomParser :: Parser r String (Parser r' String AST)
atomParser = iLeafParser
    <|> dLeafParser
    <|> sLeafParser
    <|> eoiParser
    <|> charClassParser

starFreeParser :: Parser r String (Parser r' String AST)
starFreeParser = (consumeSingle '(' >> cfexParser << consumeSingle ')') <|> atomParser

concatFreeParser :: Parser r String (Parser r' String AST) 
concatFreeParser = do
    sf <- starFreeParser
    ((consumeSingle '*') >> return (Node "many" <$> many sf)) 
        <|> ((consumeSingle '+') >> return (Node "some" <$> some sf)) 
        <|> return sf


cfexParser :: Parser r String (Parser r' String AST)
cfexParser = (\ps -> Node "concat" <$> sequenceA ps) <$> some concatFreeParser

parserParser :: Parser r String (Parser r' String AST)
parserParser = cfexParser << eoi

specParse :: String -> String -> Either String AST
specParse g i = doParse parserParser g >>= (`doParse` i)