{-|
Module      : Text.LParse.Parser
Description : Core for LParse
Copyright   : (c) Marcus Völker, 2017-2019
License     : MIT
Maintainer  : marcus.voelker@rwth-aachen.de

This module implements LParse's core: The parser data structure, instances of the important typeclasses and functions to run the parser
-}
module Text.LParse.Parser where

import Control.DoubleContinuations

import Control.Applicative
import Control.Arrow
import qualified Control.Category as C
import Control.Monad

import Data.Either
import Data.List

-- | Error structure for handling parser failure
data ParserError e = EmptyAlternative | UnexpectedToken String | UnexpectedEOI | UnconsumedTokens | UnfulfilledCondition String | UserError e

instance (Show e) => Show (ParserError e) where
    show EmptyAlternative = "Empty Alternative"
    show (UnexpectedToken t) = "Expected Tokens `" ++ t ++ "'"
    show UnexpectedEOI = "Unexpected end of input"
    show UnconsumedTokens = "Input not fully consumed"
    show (UnfulfilledCondition e) = "Unfulfilled condition: " ++ e
    show (UserError e) = show e

-- | The Parser structure itself wraps a function from a collection of tokens (collectively of type t) to a double continuation giving
-- back a string in case of an error (the error message) and a pair (a,t) in case of a success (the parsing result and rest of the input)
data Parser r e t a = Parser {pFunc :: t -> DCont r (ParserError e) (a,t)}

-- | via Monad/Functor laws
instance Functor (Parser r e t) where
    fmap = liftM

-- | via Monad/Applicative laws
instance Applicative (Parser r e t) where
    pure = return
    f <*> a = f >>= (<$> a)

-- | an empty parser in the sense of Alternative always fails and throws nothing. Branching between parsers means trying both in a row and
-- taking the first one that succeeds
instance Alternative (Parser r e t) where
    empty = Parser (const $ throw EmptyAlternative)
    p1 <|> p2 = Parser ((<|>) <$> pFunc p1 <*> pFunc p2)

-- | returning a value means building a parser that consumes no input and just gives back the value (i.e. always succeeds)
-- the bind operator means using the parser, creating a second parser from the result (with the given function) and then parsing with that.
-- Both parsers successively consume input, i.e. @consume "a" >>= (const $ consume "b")@ will consume the string "ab"
instance Monad (Parser r e t) where
    return a = Parser (\s -> return (a,s))
    a >>= f = Parser (pFunc a >=> (\(r, s') -> pFunc (f r) s'))

-- | Defined via Alternative
instance MonadPlus (Parser r e t) where
    mzero = empty
    mplus = (<|>)

-- | @MonadFix@-analogue for @Parser@, using the @DCont@ function @dfix@
pfix :: (a -> Parser (Either (ParserError e) (a,t)) e t a) -> Parser r e t a
pfix f = Parser (dfix . flip (pFunc . f . fst . fromRight undefined)) 

-- | The identity parser returns the input. Concatenating two parsers means using the parsing result of the first as tokens for the second
instance C.Category (Parser r e) where
    id = Parser (\s -> return (s,s))
    (.) b a = Parser (\s -> DCont (\btr etr -> run (pFunc a s) (\(x,r) -> run (pFunc b x) (\(y,_) -> btr (y,r)) etr) etr))

-- | Lifting a function to an arrow applies the function to the input. (***) executes two parsers in parallel, giving both results as a pair 
-- (but only if both succeed)
instance Arrow (Parser r e) where
    arr f = Parser (\s -> return (f s, undefined))
    (***) p1 p2 = Parser (\(a,b) -> DCont (\btr etr -> run (pFunc p1 a) (\(a',ra) -> run (pFunc p2 b) (\(b',rb) -> btr ((a',b'),(ra,rb))) etr) etr))

-- | Runs the parser on the tokens and returns whether the parse succeeded. Results are discarded.
check :: Parser Bool e t a -> t -> Bool
check p s = parse p s (const True) (const False)

-- | Runs the parser on the tokens, using two functions to run the contained continuation
parse :: Parser r e t a -> t -> (a -> r) -> (ParserError e -> r) -> r
parse p s = run (pFunc p s) . (. fst)

-- | Same as @parse@, but giving back the results via @Either@
doParse :: Parser (Either (ParserError e) a) e t a -> t -> Either (ParserError e) a
doParse p s = invoke (fst <$> pFunc p s)

-- | Same as @parse@, but assuming the parsing succeeds, hard failing via @undefined@ otherwise
forceParse :: Parser a e t a -> t -> a
forceParse p s = parse p s id undefined 

-- | Runs the parser and prints the results
debugParse :: (Show a, Show e) => Parser (IO ()) e t a -> t -> IO ()
debugParse p s = debugParse' p s (putStr . (\x -> show x ++ "\n"))

-- | Runs the parser and prints the results via a custom printing function
debugParse' :: (Show e) => Parser (IO ()) e t a -> t -> (a -> IO()) ->  IO ()
debugParse' p s a = run (pFunc p s) (a . fst) (\e -> putStr ("Error: "++ show e ++ "\n"))