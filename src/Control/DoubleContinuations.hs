{-|
Module      : Control.DoubleContinuations 
Description : Continuations that can succeed or fail
Copyright   : (c) Marcus Völker, 2017
License     : MIT
Maintainer  : marcus.voelker@rwth-aachen.de

This module describes DoubleContinuations, which are Continuations that may have succeeded or failed.
Instead of just taking a single function (a -> r) -> r to execute after the computation has run,
a double continuation takes two functions: one to call in case of success and one to call in case of error
This allows for easy implementation of exception handling and structuring control flow in a pass/fail manner
-}
module Control.DoubleContinuations where 

import Control.Applicative
import Control.Monad
import Data.Either

-- | The double continuation. Takes two functions, one to invoke if the computation is successful, one if it errors
data DCont r e a = DCont {run :: (a -> r) -> (e -> r) -> r}

-- | Generates a continuation that always fails. For a continuation that always succeeds, see return
throw :: e  -- ^ The error to return
    -> DCont r e a
throw x = DCont (\_ g -> g x)

-- | Binding a Continuation means running it, then feeding the result into f to generate a new continuation, and running that
instance Monad (DCont r e) where
    return x = DCont (\f _ -> f x)
    c >>= f = DCont (\btr etr -> run c (\x -> run (f x) btr etr) etr)

dfix :: (Either e a -> DCont (Either e a) e a) -> DCont r e a
dfix f = let ea = run (f ea) Right Left in wrap ea

-- | via Monad/Functor laws
instance Functor (DCont r e) where
    fmap = liftM

-- | via Monad/Applicative laws
instance Applicative (DCont r e) where
    pure = return
    f <*> a = f >>= (<$> a)

-- | An empty alternative just fails with an undefined error. Branching means first trying one, and in case of failure, the other
instance Alternative (DCont r e) where
    empty = throw undefined
    p1 <|> p2 = DCont (\atr etr -> run p1 atr (\_ -> run p2 atr etr))

-- | Convenience function to run a computation and put the result into an Either (with Left being the error and Right being the success)
invoke :: DCont (Either e a) e a -> Either e a
invoke c = run c Right Left

-- | Convenience function to put an @Either@ into a @DCont@
wrap :: Either e a -> DCont r e a
wrap = either throw return