module Main where 

import Text.LParse.Parser
import Text.LParse.Atomics
import Text.LParse.Transformers

import Control.Applicative
import Control.Arrow
import Control.Monad
import Data.Either
import Data.List
import Data.Maybe
import System.Exit (exitSuccess,exitFailure)

bracks :: Parser r String ()
bracks = (consume "(" >> nesting << consume ")")
    <|> (consume "[" >> nesting << consume "]")
    <|> (consume "{" >> nesting << consume "}")
    <|> (consume "<" >> nesting << consume ">")

nesting :: Parser r String ()
nesting = void $ many bracks

succCases :: [(Parser r String (),String)]
succCases = [
    (noop,""),
    (eoi,""),
    (discard,"lel"),
    (discard >> eoi,"lorem ipsum"),
    (consume "prefix","prefixed"),
    (consume "", "foo"),
    (consume "", ""),
    (letter >> eoi, "b"),
    (digit >> eoi, "4"),
    (word >> eoi, "banana"),
    (nesting >> eoi, "({()}[])"),
    (try word >> integer >> eoi, "123"),
    (try word >> integer >> eoi, "super123")
    ]

failCases :: [(Parser r String (),String)]
failCases = [
    (eoi,"foo"),
    (consume "prefix", "freepix"),
    (consume "prefix", ""),
    (letter >> eoi, "banana"),
    (digit >> eoi, "42"),
    (word >> eoi, "banana bread"),
    (nesting >> eoi, "({(})[])"),
    (void $ nParse (=='1') integer "Expected '1'", "234")
    ]

stringCases :: [(Parser r String String, String, String)]
stringCases = [
    (word,"sufficient example","sufficient"),
    (integer >>> (show <$> bDigits 2), "19", "[1,1,0,0,1]")
    ]

intCases :: [(Parser r String Integer, String, Integer)]
intCases = [
    (integer,"123 is a nice number",123),
    (digit,"123 is a nice number",1),
    (sum <$> sepMany (consume " ") integer,"1 4 12 61 192",1+4+12+61+192),
    (integer >>> (sum <$> bDigits 2), "19", 3),
    (integer >>> (foldr (\x y -> x + y * 2) 0 <$> bDigits 2), "19", 19),
    ((\x y -> x*10+y) <$> sInteger <*> ((consumeSingle ' ') >> sInteger), "-123 123", (-123*10) + 123),
    (nParse (=='1') integer "Expected '1'", "123", 123)
    ]

runTests :: [(Parser (Either String a) t a,t)] -> [Either String a]
runTests = map (uncurry doParse)

eqTest :: (Eq a, Show a) => (Parser (Either String ()) t a, t, a) -> Either String ()
eqTest (p,i,e) = parse p i (\r -> if r == e then Right () else Left ("Expected " ++ show e ++ ", but got " ++ show r)) (\e -> Left $ "Parser error: " ++ e)

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