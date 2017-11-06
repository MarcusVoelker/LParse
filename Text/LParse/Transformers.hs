module Text.LParse.Transformers where 

import Control.Continuations
import Text.LParse.Parser

import Control.Applicative

(<<) :: (Monad m) => m a -> m b -> m a
a << b = a >>= ((b >>) . return)

cParse :: (t -> Bool) -> Parser r t a -> String -> Parser r t a
cParse c p err = Parser (\s -> if c s then pFunc p s else throw err)

pParse :: (t -> t) -> Parser r t a -> Parser r t a
pParse f p = Parser (pFunc p . f)

sepSome :: Parser r t () -> Parser r t a -> Parser r t [a]
sepSome sep p = ((:) <$> p <*> many (sep >> p)) <|> fmap return p

sepMany :: Parser r t () -> Parser r t a -> Parser r t [a]
sepMany sep p = sepSome sep p <|> return []

skip :: (Eq t) => [t] -> Parser r [t] a -> Parser r [t] a
skip s = pParse (filter (not . (`elem` s)))

skipWhitespace :: Parser r String a -> Parser r String a
skipWhitespace = skip " \n\r\t"