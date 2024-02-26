module Advent.Util.Funct ( (<$*>) ) where


(<$*>) :: Functor f => f (a -> b) -> a -> f b
x <$*> y = fmap (\m -> m y) x
