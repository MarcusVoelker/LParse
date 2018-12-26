{-|
Module      : Text.LParse.TokenStream
Description : Underlying data structure for sequential parsing
Copyright   : (c) Marcus Völker, 2017-2018
License     : MIT
Maintainer  : marcus.voelker@rwth-aachen.de

This module contains the `TokenStream` class, an abstraction of lists, similar to `Traversable`, but geared for use with LParse
-}
module Text.LParse.TokenStream where 

import Data.Either
import Data.Maybe
import Data.Traversable

import Prelude hiding (filter,zip,zipWith,drop)

-- | `TokenStream` abstracts a list, i.e., something that has a next element to process and a rest afterwards
class (Functor t, Foldable t) => TokenStream t where
    -- | `top` gives the next element to process. Similar to `head`
    top :: t a -> a
    -- | `rest` gives what is left after processing `top`. Similar to `tail`
    rest :: t a -> t a
    -- | `nil` gives the empty `TokenStream`. Similar to `[]`
    nil :: t a
    -- | `cons` prepends an element to the `TokenStream`. Similar to `(:)`
    cons :: a -> t a -> t a

instance TokenStream [] where
    top = head
    rest = tail
    nil = []
    cons = (:)

instance TokenStream Maybe where
    top = fromJust
    rest = const Nothing
    nil = Nothing
    cons a _ = Just a

instance TokenStream (Either a) where
    top = head . rights . return
    rest x = if isLeft x then x else nil
    nil = Left undefined
    cons a _ = Right a

-- | `TokenStream` version of `drop` 
drop ::  (TokenStream s) => Int -> s a -> s a
drop 0 x = x
drop n x = rest $ drop (n-1) x

-- | `TokenStream` version of `zip`
zip :: (TokenStream s) => s a -> s b -> s (a,b)
zip = zipWith (,)

-- | `TokenStream` version of `zipWith`
zipWith :: (TokenStream s) => (a -> b -> c) -> s a -> s b -> s c
zipWith f l r | null l || null r = nil
               | otherwise      = f (top l) (top r) `cons` zipWith f (rest l) (rest r)

-- | `TokenStream` version of `filter`
filter :: (TokenStream s) => (a -> Bool) -> s a -> s a
filter c x | null x = nil
            | c (top x) = top x `cons` filter c (rest x)
            | otherwise = filter c (rest x)