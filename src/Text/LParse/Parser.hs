module Text.LParse.Parser where

import Control.Continuations

import Control.Applicative
import Control.Monad
import Control.Arrow
import qualified Control.Category as C
import Data.List

data Parser r t a = Parser {pFunc :: t -> DCont r String (a,t)}

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

instance MonadPlus (Parser r t) where
    mzero = empty
    mplus = (<|>)

instance C.Category (Parser r) where
    id = Parser (\s -> return (s,s))
    (.) b a = Parser (\s -> DCont (\btr etr -> run (pFunc a s) (\(x,r) -> run (pFunc b x) (\(y,_) -> btr (y,r)) etr) etr))

instance Arrow (Parser r) where
    arr f = Parser (\s -> return (f s, undefined))
    (***) p1 p2 = Parser (\(a,b) -> DCont (\btr etr -> run (pFunc p1 a) (\(a',ra) -> run (pFunc p2 b) (\(b',rb) -> btr ((a',b'),(ra,rb))) etr) etr))


parse :: Parser r t a -> t -> (a -> r) -> (String -> r) -> r
parse p s = run (pFunc p s) . (. fst)

doParse :: Parser (Either String a) t a -> t -> Either String a
doParse p s = invoke (fst <$> pFunc p s)

debugParse :: (Show a) => Parser (IO ()) t a -> t -> IO ()
debugParse p s = debugParse' p s (putStr . (\x -> show x ++ "\n"))

debugParse' :: (Show a) => Parser (IO ()) t a -> t -> (a -> IO()) ->  IO ()
debugParse' p s a = run (pFunc p s) (a . fst) (\e -> putStr ("Error: "++ e ++ "\n"))