module Main where 

import Text.LParse.Parser
import Text.LParse.Atomics
import Text.LParse.Transformers

import Control.Applicative
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
    (consume "", ""),
    (word >> eoi, "banana")
    ]

failCases = [
    (eoi,"foo"),
    (consume "prefix", "freepix"),
    (consume "prefix", ""),
    (word >> eoi, "banana bread")
    ]

stringCases = [
    (word,"sufficient example","sufficient")
    ]

intCases = [
    (integer,"123 is a nice number",123),
    (sum <$> sepMany (consume " ") integer,"1 4 12 61 192",1+4+12+61+192)
    ]

runTests = map (uncurry doParse)


eqTest (p,i,e) = parse p i (\r -> if r == e then Right () else Left ("Expected " ++ show e ++ ", but got " ++ show r)) (const $ Left "Parser error")

main = 
    let sres = runTests succCases in
    let fres = runTests failCases in
    let seres = map eqTest stringCases in
    let ieres = map eqTest intCases in
    if all isRight sres && all isLeft fres && all isRight seres && all isRight ieres then
        exitSuccess
    else if any isLeft sres then
        putStrLn (head $ lefts sres) >> exitFailure
    else if any isRight fres then
        putStrLn "Fail case succeeded" >> exitFailure
    else if any isLeft seres then
        putStrLn (head $ lefts seres) >> exitFailure
    else if any isLeft ieres then
        putStrLn (head $ lefts ieres) >> exitFailure
    else
        putStrLn "Unknown internal error" >> exitFailure