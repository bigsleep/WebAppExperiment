{-# LANGUAGE TypeOperators, FlexibleContexts #-}
module Whone.Random
( getRandom
, getRandomR
) where

import Whone.Internal
import Control.Monad.Free (Free(..))

data IRandom x a =
    GetRandom (x -> a) |
    GetRandomR (x, x) (x -> a)

instance Functor (IRandom x) where
    fmap f (GetRandom g) = GetRandom (f . g)
    fmap f (GetRandomR r g) = GetRandomR r (f . g)

getRandom :: (IRandom x :<: f) => App f x
getRandom = App . inject . GetRandom $ Pure

getRandomR :: (IRandom x :<: f) => (x, x) -> App f x
getRandomR r = App . inject $ GetRandomR r Pure
