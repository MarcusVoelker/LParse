module Text.LParse.Transformers where 

import Control.Continuations
import Text.LParse.Parser

import Control.Applicative

(<<) :: (Monad m) => m a -> m b -> m a
a << b = a >>= ((b >>) . return)

cParse :: ([t] -> Bool) -> Parser r t a -> String -> Parser r t a
cParse c p err = Parser (\s -> if c s then pFunc p s else throw err)

pParse :: ([t] -> [t]) -> Parser r t a -> Parser r t a
pParse f p = Parser (pFunc p . f)

sepSome :: Parser r t () -> Parser r t a -> Parser r t [a]
sepSome sep p = ((:) <$> p <*> many (sep >> p)) <|> fmap return p <|> return []