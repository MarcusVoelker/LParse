{-|
Module      : Text.LParse.Transformers
Description : Parser transformers for use with LParse
Copyright   : (c) Marcus Völker, 2017
License     : MIT
Maintainer  : marcus.voelker@rwth-aachen.de

This module implements LParse's transformers, i.e. functions that build new parsers from other parsers
-}
module Text.LParse.Transformers where 

import Control.DoubleContinuations
import Text.LParse.Parser
import Text.LParse.TokenStream

import Control.Applicative
import Data.Char

-- | Executes components in the same order as @(>>)@, but returning the first rather than the second monad. Note that @a >> b /= b << a@
(<<) :: (Monad m) => m a -> m b -> m a
a << b = a >>= ((b >>) . return)

-- | Takes a condition the parser's input has to fulfil in order for the parser to succeed
cParse :: (t -> Bool) -> Parser r t a -> String -> Parser r t a
cParse c p err = Parser (\s -> if c s then pFunc p s else throw err)

-- | Takes  condition the next token has to fulfil in order for the parser to succeed
nParse :: (TokenStream s, Eq (s t)) => (t -> Bool) -> Parser r (s t) a -> String -> Parser r (s t) a
nParse c = cParse (\s -> nil /= s && c (top s))

-- | Transforms the input before applying the parser
pParse :: (t -> t) -> Parser r t a -> Parser r t a
pParse f p = Parser (pFunc p . f)

-- | Takes a parser that consumes separators and a parser that consumes the desired data and returns a non-empty list of desired data (separated by the separator in source)
-- For example: @sepSome (consume " ") word@ applied to @"a banana is tasty"@ returns @["a","banana","is","tasty"]@
sepSome :: Parser r t () -> Parser r t a -> Parser r t [a]
sepSome sep p = ((:) <$> p <*> many (sep >> p)) <|> fmap return p

-- | Same as @sepSome@, but allows empty lists
sepMany :: Parser r t () -> Parser r t a -> Parser r t [a]
sepMany sep p = sepSome sep p <|> return []

-- | Removes all tokens from the given list from the input
skip :: (Eq t, TokenStream s) => [t] -> Parser r (s t) a -> Parser r (s t) a
skip s = skipBy (not . (`elem` s))

-- | Same as skip, but with a custom comparator
skipBy :: (TokenStream s) => (t -> Bool) -> Parser r (s t) a -> Parser r (s t) a
skipBy f = pParse (sFilter f)

-- | Skips standard whitespace characters from a String input
skipWhitespace :: Parser r String a -> Parser r String a
skipWhitespace = skipBy (not . isSpace)

-- | Replaces the first token by applying the given function
replace :: (TokenStream s) => (t -> t) -> Parser r (s t) a -> Parser r (s t) a
replace f p = Parser (pFunc p . (\x -> f (top x) `cons` rest x))

-- | Tries to run the given parser, giving back Just result or Nothing
try :: (TokenStream s) => Parser r (s t) a -> Parser r (s t) (Maybe a)
try p = (Just <$> p) <|> return Nothing