module Text.LParse.Atomics where 

import Control.Continuations

import Text.LParse.Parser

noopParse :: Parser r t ()
noopParse = Parser (\s -> DCont (\btr _ -> btr ((),s)))

tokenParse :: (t -> a) -> Parser r t a
tokenParse f = Parser (\s -> DCont (\btr etr -> if null s then etr "Unexpected EOI" else btr (f $ head s,tail s)))

consume :: (Eq t, Show t) => [t] -> Parser r t ()
consume pre = cParse (all id . zipWith (==) pre) (pParse (drop (length pre)) noopParse) ("Expected " ++ show pre)

consumeSingle :: (Eq t, Show t) => t -> Parser r t ()
consumeSingle t = cParse (\s -> not (null s) && head s == t) (pParse tail noopParse) ("Expected " ++ show t)
