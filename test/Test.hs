module Main where 

import Text.LParse.Parser
import Text.LParse.Atomics
import Text.LParse.Transformers

import Control.Monad
import Data.Either
import Data.List
import Data.Maybe
import System.Exit (exitSuccess,exitFailure)

succCases = [
    (noop,""),
    (eoi,""),
    (discard,"lel"),
    ((discard >> eoi),"lorem ipsum"),
    ((consume "prefix"),"prefixed")
    ]

main = 
    let res = map (uncurry doParse) succCases in
    if all isRight res then
        exitSuccess
    else
        putStrLn (head $ lefts res) >> exitFailure