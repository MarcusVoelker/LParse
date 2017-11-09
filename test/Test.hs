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
    (discard >> eoi,"lorem ipsum"),
    (consume "prefix","prefixed"),
    (consume "", "foo"),
    (consume "", "")
    ]

failCases = [
    (eoi,"foo"),
    (consume "prefix", "freepix"),
    (consume "prefix", "")
    ]

runTests = map (uncurry doParse)

main = 
    let sres = runTests succCases in
    let fres = runTests failCases in
    if all isRight sres && all isLeft fres then
        exitSuccess
    else if any isLeft sres then
        putStrLn (head $ lefts sres) >> exitFailure
    else 
        putStrLn "Fail case succeeded" >> exitFailure