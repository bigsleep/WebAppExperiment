{-# LANGUAGE TypeOperators, FlexibleContexts, ExistentialQuantification #-}
module Whone.Random
( IRandom(..)
, getRandom
, getRandomR
) where

import Whone.Internal
import System.Random (Random)
import Control.Monad.Trans.Free (liftF)

data IRandom a =
    forall x. (Random x) => GetRandom (x -> a) |
    forall x. (Random x) => GetRandomR (x, x) (x -> a)

instance Functor IRandom where
    fmap f (GetRandom g) = GetRandom (f . g)
    fmap f (GetRandomR r g) = GetRandomR r (f . g)

getRandom :: (Random x, Monad m, IRandom :<: f) => App f m x
getRandom = App . liftF . inject . GetRandom $ id

getRandomR :: (Random x, Monad m, IRandom :<: f) => (x, x) -> App f m x
getRandomR r = App . liftF . inject . GetRandomR r $ id
