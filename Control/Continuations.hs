module Control.Continuations where 

import Control.Applicative
import Control.Monad

data DCont r e a = DCont {run :: (a -> r) -> (e -> r) -> r}

throw :: e -> DCont r e a
throw x = DCont (\_ g -> g x)

instance Monad (DCont r e) where
    return x = DCont (\f _ -> f x)
    c >>= f = DCont (\btr etr -> run c (\x -> run (f x) btr etr) etr)

instance Functor (DCont r e) where
    fmap = liftM

instance Applicative (DCont r e) where
    pure = return
    f <*> a = f >>= (<$> a)

instance Alternative (DCont r e) where
    empty = DCont (\_ g -> g undefined)
    p1 <|> p2 = DCont (\atr etr -> run p1 atr (\_ -> run p2 atr etr))