{-|
Module      : Text.LParse.Atomics
Description : Atomic parsers for use with LParse
Copyright   : (c) Marcus Völker, 2017
License     : MIT
Maintainer  : marcus.voelker@rwth-aachen.de

This module implements LParse's atomic parsers, i.e., parsers that are not built up from other parsers.
-}
module Text.LParse.Atomics where 

import Control.DoubleContinuations

import Control.Applicative
import Control.Monad
import Data.Char
import Text.LParse.Parser
import Text.LParse.Transformers

-- | A parser that always succeeds, parses nothing and returns unit
noop :: Parser r t ()
noop = return ()

-- | A parser that consumes the whole input and returns it unchanged
full :: Parser r [t] [t]
full = many tokenReturn

-- | A parser that consumes the whole input and discards it, successfully
discard :: Parser r [t] ()
discard = void full

-- | A parser that parses nothing, but only succeeds if the input is empty
eoi :: Parser r [t] ()
eoi = cParse null noop ("Input not fully consumed")

-- | Extracts the first token from the input and applies the given function to it
tokenParse :: (t -> a) -> Parser r [t] a
tokenParse f = Parser (\s -> DCont (\btr etr -> if null s then etr "Unexpected EOI" else btr (f $ head s,tail s)))

-- | Consumes and returns the first token of the input
tokenReturn :: Parser r [a] a
tokenReturn = tokenParse id

-- | Succeeds exactly if the input begins with the given sequence. On success, consumes that sequence
consume :: (Eq t, Show t) => [t] -> Parser r [t] ()
consume pre = cParse ((&&) <$> (all id . zipWith (==) pre) <*> ((>= length pre) . length)) (pParse (drop (length pre)) noop) ("Expected " ++ show pre)

-- | Succeeds exactly if the input begins with the given token. On success, consumes that token
consumeSingle :: (Eq t, Show t) => t -> Parser r [t] ()
consumeSingle t = cParse (\s -> not (null s) && head s == t) (pParse tail noop) ("Expected " ++ show t)

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

-- | Succeeds if the first token matches the given function, without consuming it
peek :: (t -> Bool) -> String -> Parser r [t] ()
peek c = cParse (c . head) noop