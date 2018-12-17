module Text.LParse.Metaparser (specParse,metaParser,AST) where

import Control.Applicative
import Control.Arrow

import Text.LParse.Parser
import Text.LParse.Prebuilt

data Token = Literal Char | CharClass String | Integer | Digit | Word | Star | Plus | May | Eoi | LParen | RParen deriving (Show,Eq)
data AST = Node String [AST] | ILeaf Integer | SLeaf String | EOI deriving Show

escaped :: Parser r String Token
escaped = consumeReturn 'i' Integer
    <|> consumeReturn 'd' Digit
    <|> consumeReturn 'w' Word
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
iLeafParser = consumeReturn Integer (ILeaf <$> integer)

dLeafParser :: Parser r [Token] (Parser r' String AST)
dLeafParser = consumeReturn Digit (ILeaf <$> digit)

sLeafParser :: Parser r [Token] (Parser r' String AST)
sLeafParser = consumeReturn Word (SLeaf <$> word)

eoiParser :: Parser r [Token] (Parser r' String AST)
eoiParser = consumeReturn Eoi (eoi >> return EOI)

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

charParser :: Parser r [Token] (Parser r' String AST)
charParser = nParse isLiteral (tokenParse (fmap (SLeaf . return) . (\c -> consumeReturn c c) . getLiteral)) "Expected Literal"

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


cfexParser :: Parser r [Token] (Parser r' String AST)
cfexParser = (\ps -> Node "concat" <$> sequenceA ps) <$> some concatFreeParser

parserParser :: Parser r [Token] (Parser r' String AST)
parserParser = cfexParser << eoi

metaParser :: Parser r String (Parser r' String AST)
metaParser = metaTokenizer >>> parserParser

specParse :: String -> String -> Either String AST
specParse g i = doParse metaParser g >>= (`doParse` i)