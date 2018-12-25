module Main where 

import Text.LParse.Parser
import Text.LParse.Prebuilt
import Text.LParse.Metaparser

import Control.Applicative
import Control.Arrow
import Control.Monad
import Data.Either
import Data.List
import Data.Maybe
import System.Exit (exitSuccess,exitFailure)

bracks :: Parser r String ()
bracks = surround "()" nesting
    <|> surround "[]" nesting
    <|> surround "{}" nesting
    <|> surround "<>" nesting

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
    ((\x y -> x*10+y) <$> sInteger <*> (consumeSingle ' ' >> sInteger), "-123 123", (-123*10) + 123),
    (nParse (=='1') integer "Expected '1'", "123", 123)
    ]

metaCases :: [(String, String, String)]
metaCases = [
        ("\\w$","oha","concat(oha,$)"),
        ("\\d*$","123123","concat(many(1,2,3,1,2,3),$)"),
        ("(\\w\\d)+$","abc3def1g0","concat(some(concat(abc,3),concat(def,1),concat(g,0)),$)"),
        ("abc\\d$","abc3","concat(a,b,c,3,$)"),
        ("t::=abc;%t$","abc","concat(concat(a,b,c),$)"),
        ("t::=a|c;%t$","a","concat(a,$)"),
        ("t::=a%t|c;%t$","aaac","concat(concat(a,concat(a,concat(a,c))),$)")
    ]

runTests :: [(Parser (Either String a) t a,t)] -> [Either String a]
runTests = map (uncurry doParse)

eqTest :: (Eq a, Show a) => (Parser (Either String ()) t a, t, a) -> Either String ()
eqTest (p,i,e) = parse p i (\r -> if r == e then Right () else Left ("Expected " ++ show e ++ ", but got " ++ show r)) (\e -> Left $ "Parser error: " ++ e)

succTest :: [Either String a] -> IO ()
succTest res = unless (all isRight res) $ putStrLn (head $ lefts res) >> exitFailure

failTest :: [Either String a] -> IO ()
failTest res = unless (all isLeft res) $ putStrLn "Fail Test Succeeded" >> exitFailure

metaTest :: (String,String,String) -> Either String AST
metaTest (g,i,a) = either (\a -> Left ("Case " ++ show (g,i)  ++ ": " ++ a)) (\ast -> if show ast == a then Right ast else Left ("Expected AST " ++ a ++ " but got " ++ show ast)) (specParse g i)

main ::IO ()
main = do
    let sres = runTests succCases
    let fres = runTests failCases
    let seres = map eqTest stringCases
    let ieres = map eqTest intCases
    let mres = map metaTest metaCases
    succTest sres
    failTest fres
    succTest seres
    succTest ieres
    succTest mres
    exitSuccess