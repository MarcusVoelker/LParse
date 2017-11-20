module Text.LParse.TokenStream where 

import Data.Maybe
import Data.Traversable

class (Functor t, Foldable t) => TokenStream t where
    top :: t a -> a
    rest :: t a -> t a
    nil :: t a
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

skipN :: (TokenStream s) => Int -> s a -> s a
skipN 0 x = x
skipN n x = rest $ skipN (n-1) x

sZip :: (TokenStream s) => s a -> s b -> s (a,b)
sZip = sZipWith (,)

sZipWith :: (TokenStream s) => (a -> b -> c) -> s a -> s b -> s c
sZipWith f l r | null l || null r = nil
               | otherwise      = f (top l) (top r) `cons` (sZipWith f (rest l) (rest r))

sFilter :: (TokenStream s) => (a -> Bool) -> s a -> s a
sFilter c x | null x = nil
            | c (top x) = (top x) `cons` sFilter c (rest x)
            | otherwise = sFilter c (rest x)