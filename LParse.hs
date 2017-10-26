module LParse where


import Continuations

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

(<<) :: (Monad m) => m a -> m b -> m a
a << b = a >>= ((b >>) . return)

parse :: Parser r t a -> [t] -> (a -> r) -> (String -> r) -> r
parse p s btr = run (pFunc p s) (btr . fst)

debugParse :: (Show a) => Parser (IO ()) t a -> [t] -> IO ()
debugParse p s = run (pFunc p s) (putStr . (\x -> show (fst x) ++ "\n")) (\e -> putStr ("Error: "++ e ++ "\n"))

debugParse' :: (Show a) => Parser (IO ()) t a -> [t] -> (a -> IO()) ->  IO ()
debugParse' p s a = run (pFunc p s) (a . fst) (\e -> putStr ("Error: "++ e ++ "\n"))

cParse :: ([t] -> Bool) -> Parser r t a -> String -> Parser r t a
cParse c p err = Parser (\s -> if c s then pFunc p s else throw err)

pParse :: ([t] -> [t]) -> Parser r t a -> Parser r t a
pParse f p = Parser (pFunc p . f)

noopParse :: Parser r t ()
noopParse = Parser (\s -> DCont (\btr _ -> btr ((),s)))

tokenParse :: (t -> a) -> Parser r t a
tokenParse f = Parser (\s -> DCont (\btr etr -> if null s then etr "Unexpected EOI" else btr (f $ head s,tail s)))

consume :: (Eq t, Show t) => [t] -> Parser r t ()
consume pre = cParse (all id . zipWith (==) pre) (pParse (drop (length pre)) noopParse) ("Expected " ++ show pre)

consumeSingle :: (Eq t, Show t) => t -> Parser r t ()
consumeSingle t = cParse (\s -> not (null s) && head s == t) (pParse tail noopParse) ("Expected " ++ show t)

sepSome :: Parser r t () -> Parser r t a -> Parser r t [a]
sepSome sep p = ((:) <$> p <*> many (sep >> p)) <|> fmap return p <|> return []