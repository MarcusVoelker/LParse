{-|
Module      : Text.LParse.Prebuilt
Description : Core for LParse
Copyright   : (c) Marcus Völker, 2017-2018
License     : MIT
Maintainer  : marcus.voelker@rwth-aachen.de

This module contains prebuilt parsers that fulfill a certain job (formerly in @Text.LParse.Atomics@) and parser transformers that one or more parsers and modify/combine them in a certain way (formerly in @Text.LParse.Transformers@)

Some of these parsers depend on their input being given in the form of a @TokenStream@.
-}

module Text.LParse.Prebuilt where


import Control.Applicative
import Control.DoubleContinuations
import Control.Monad
import Data.Char

import Text.LParse.Parser
import qualified Text.LParse.TokenStream as T
import Text.LParse.TokenStream (TokenStream,top,rest,nil,cons)

-- * Atomic Parsers

-- ** Empty Parsers

-- | A parser that always succeeds, parses nothing and returns unit
noop :: Parser r t ()
noop = return ()

-- | Succeeds if the first token matches the given function, without consuming it
peek :: (TokenStream s) => (t -> Bool) -> String -> Parser r (s t) ()
peek c = cParse (c . top) noop

-- | A parser that parses nothing, but only succeeds if the input is empty
eoi :: Parser r [t] ()
eoi = cParse null noop "Input not fully consumed"

-- *** Consumers

-- | A parser that consumes the whole input and discards it, successfully
discard :: Parser r [t] ()
discard = void full

-- | Succeeds exactly if the input begins with the given sequence. On success, consumes that sequence
consume :: (Eq t, Show (s t), TokenStream s) => s t -> Parser r (s t) ()
consume pre = cParse ((&&) <$> (and . T.zipWith (==) pre) <*> ((>= length pre) . length)) (pParse (T.drop (length pre)) noop) ("Expected " ++ show pre)

-- | Consumes exactly the given input and then returns the given constant result
consumeReturn :: (Eq t, Show (s t), TokenStream s) => s t -> a -> Parser r (s t) a
consumeReturn t a = consume t >> return a

-- | Succeeds exactly if the input begins with the given token. On success, consumes that token
consumeSingle :: (Eq t, Show t, TokenStream s) => t -> Parser r (s t) ()
consumeSingle t = cParse (\s -> not (null s) && top s == t) (pParse rest noop) ("Expected " ++ show t)

-- | Consumes exactly the given token and then returns the given constant result
consumeSReturn :: (Eq t, Show t, TokenStream s) => t -> a -> Parser r (s t) a
consumeSReturn t a = consumeSingle t >> return a

-- ** Input Transformers

-- *** Identity Transformers

-- | Consumes and returns the first token of the input
tokenReturn :: (TokenStream s) => Parser r (s a) a
tokenReturn = tokenParse id

-- | A parser that consumes the whole input and returns it unchanged
full :: Parser r [t] [t]
full = many tokenReturn

-- *** Arbitrary transformers

-- | Extracts the first token from the input and applies the given function to it
tokenParse :: (TokenStream s) => (t -> a) -> Parser r (s t) a
tokenParse f = Parser (\s -> DCont (\btr etr -> if null s then etr "Unexpected EOI" else btr (f $ top s,rest s)))

-- | A parser that always succeeds with the given function
success :: (t -> (a,t)) -> Parser r t a
success = Parser . (return .)

-- ** Entity Parsers

-- | Extracts the first digit and returns it
digit :: Parser r String Integer
digit = read . return <$> cParse (\s -> not (null s) && isDigit (head s)) tokenReturn "Expected digit"

-- | Extracts the first digit and returns it
letter :: Parser r String Char
letter = cParse (\s -> not (null s) && isLetter (head s)) tokenReturn "Expected letter"

-- | Extracts the first word (i.e. contiguous string of letters) from the input and returns it
word :: Parser r String String
word = some letter 

-- | Extracts the first integer (i.e. contiguous string of digits) from the input and returns it
integer :: Parser r String Integer
integer = foldl (\x y -> x*10+y) 0 <$> some digit

-- | Extracts the first signed integer (i.e. contiguous string of digits) from the input and returns it
sInteger :: Parser r String Integer
sInteger = (\m i -> case m of (Just _) -> -i; Nothing -> i) <$> try (consumeSingle '-') <*> integer

-- | Parses an integer by removing a single digit in the given base from it. Zero is considered to have no digits
bDigit :: Integer -> Parser r Integer Integer
bDigit b = cParse (> 0) (success (\i -> (i `mod` b,i `div` b))) "Empty number!"

-- | Parses an integer by removing a single digit in the given base from it. Zero is considered to have no digits
bDigits :: Integer -> Parser r Integer [Integer]
bDigits b = many $ bDigit b

-- * Parser Transformers

-- ** Single Parser Transformers

-- | Tries to run the given parser, giving back Just result or Nothing
try :: Parser r t a -> Parser r t (Maybe a)
try p = (Just <$> p) <|> return Nothing

-- | Parses a character before and a character after the given parser, useful for parentheses
surround :: (Eq t, Show t, TokenStream s) => [t] -> Parser r (s t) a -> Parser r (s t) a
surround [l,r] p = consumeSingle l >> p << consumeSingle r

-- *** Conditional Transformers

-- | Takes a condition the parser's input has to fulfil in order for the parser to succeed
cParse :: (t -> Bool) -> Parser r t a -> String -> Parser r t a
cParse c p err = Parser (\s -> if c s then pFunc p s else throw err)

-- | Takes  condition the next token has to fulfil in order for the parser to succeed
nParse :: (TokenStream s, Eq (s t)) => (t -> Bool) -> Parser r (s t) a -> String -> Parser r (s t) a
nParse c = cParse (\s -> nil /= s && c (top s))

-- *** Input-modifying Transformers

-- | Transforms the input before applying the parser
pParse :: (t -> t) -> Parser r t a -> Parser r t a
pParse f p = Parser (pFunc p . f)

-- | Removes all tokens from the given list from the input
skip :: (Eq t, TokenStream s) => [t] -> Parser r (s t) a -> Parser r (s t) a
skip s = skipBy (not . (`elem` s))

-- | Same as skip, but with a custom comparator
skipBy :: (TokenStream s) => (t -> Bool) -> Parser r (s t) a -> Parser r (s t) a
skipBy f = pParse (T.filter f)

-- | Skips standard whitespace characters from a String input
skipWhitespace :: Parser r String a -> Parser r String a
skipWhitespace = skipBy (not . isSpace)

-- | Replaces the first token by applying the given function
replace :: (TokenStream s) => (t -> t) -> Parser r (s t) a -> Parser r (s t) a
replace f p = Parser (pFunc p . (\x -> f (top x) `cons` rest x))

-- ** Parser Combinators

-- | Executes components in the same order as @(>>)@, but returning the first rather than the second monad. Note that @a >> b /= b << a@
(<<) :: (Monad m) => m a -> m b -> m a
a << b = a >>= ((b >>) . return)

-- *** @Alternative@ variants

-- | Takes a parser that consumes separators and a parser that consumes the desired data and returns a non-empty list of desired data (separated by the separator in source)
-- For example: @sepSome (consume " ") word@ applied to @"a banana is tasty"@ returns @["a","banana","is","tasty"]@
sepSome :: Parser r t () -> Parser r t a -> Parser r t [a]
sepSome sep p = ((:) <$> p <*> many (sep >> p)) <|> fmap return p

-- | Same as @sepSome@, but allows empty lists
sepMany :: Parser r t () -> Parser r t a -> Parser r t [a]
sepMany sep p = sepSome sep p <|> return []