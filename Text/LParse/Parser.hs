module Text.LParse.Parser where

import Control.Continuations

import Control.Applicative
import Control.Monad
import Data.List

data Parser r t a = Parser {pFunc :: [t] -> DCont r String (a,[t])}

instance Functor (Parser r t) where
    fmap = liftM

instance Applicative (Parser r t) where
    pure = return
    f <*> a = f >>= (<$> a)

instance Alternative (Parser r t) where
    empty = Parser (const $ throw "Empty Fail")
    p1 <|> p2 = Parser ((<|>) <$> (pFunc p1) <*> (pFunc p2))

instance Monad (Parser r t) where
    return a = Parser (\s -> return (a,s))
    a >>= f = Parser (pFunc a >=> (\(r, s') -> pFunc (f r) s'))

parse :: Parser r t a -> [t] -> (a -> r) -> (String -> r) -> r
parse p s = run (pFunc p s) . (. fst)

debugParse :: (Show a) => Parser (IO ()) t a -> [t] -> IO ()
debugParse p s = run (pFunc p s) (putStr . (\x -> show (fst x) ++ "\n")) (\e -> putStr ("Error: "++ e ++ "\n"))

debugParse' :: (Show a) => Parser (IO ()) t a -> [t] -> (a -> IO()) ->  IO ()
debugParse' p s a = run (pFunc p s) (a . fst) (\e -> putStr ("Error: "++ e ++ "\n"))