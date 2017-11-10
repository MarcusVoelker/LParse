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

succCases :: [(Parser r String (),String)]
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

failCases :: [(Parser r String (),String)]
failCases = [
    (eoi,"foo"),
    (consume "prefix", "freepix"),
    (consume "prefix", ""),
    (word >> eoi, "banana bread")
    ]

stringCases :: [(Parser r String String, String, String)]
stringCases = [
    (word,"sufficient example","sufficient")
    ]

intCases :: [(Parser r String Integer, String, Integer)]
intCases = [
    (integer,"123 is a nice number",123),
    (sum <$> sepMany (consume " ") integer,"1 4 12 61 192",1+4+12+61+192)
    ]

runTests :: [(Parser (Either String a) t a,t)] -> [Either String a]
runTests = map (uncurry doParse)

eqTest :: (Eq a, Show a) => (Parser (Either String ()) t a, t, a) -> Either String ()
eqTest (p,i,e) = parse p i (\r -> if r == e then Right () else Left ("Expected " ++ show e ++ ", but got " ++ show r)) (const $ Left "Parser error")

succTest :: [Either String a] -> IO ()
succTest res = if all isRight res then return () else putStrLn (head $ lefts res) >> exitFailure

failTest :: [Either String a] -> IO ()
failTest res = if all isLeft res then return () else putStrLn "Fail Test Succeeded" >> exitFailure

main ::IO ()
main = do
    let sres = runTests succCases
    let fres = runTests failCases
    let seres = map eqTest stringCases
    let ieres = map eqTest intCases
    succTest sres
    failTest fres
    succTest seres
    succTest ieres
    exitSuccess