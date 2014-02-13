module Whone.Random
( getRandom
, getRandomR
) where

import Whone.Internal

data IRandom x a =
    GetRandom (x -> a) |
    GetRandomR (x, x) (x -> a)

instance Functor (IRandom x) where
    fmap f (GetRandom g) = GetRandom (f . g)
    fmap f (GetRandomR r g) = GetRandomR r (f . g)

instance Interface (IRandom x)

getRandom :: (Monad m) => App m x
getRandom = createInput (I . GetRandom)

getRandomR :: (Monad m) => (x, x) -> App m x
getRandomR r = createInput (I . GetRandomR r)
