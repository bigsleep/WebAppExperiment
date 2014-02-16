{-# LANGUAGE TypeOperators, FlexibleContexts #-}
module Whone.Random
( getRandom
, getRandomR
) where

import Whone.Internal
import Control.Monad.Trans.Free (FreeT(..), FreeF(..))

data IRandom x a =
    GetRandom (x -> a) |
    GetRandomR (x, x) (x -> a)

instance Functor (IRandom x) where
    fmap f (GetRandom g) = GetRandom (f . g)
    fmap f (GetRandomR r g) = GetRandomR r (f . g)

getRandom :: (Monad m, IRandom x :<: f) => App f m x
getRandom = App . inject . GetRandom $ FreeT . return . Pure

getRandomR :: (Monad m, IRandom x :<: f) => (x, x) -> App f m x
getRandomR r = App . inject . GetRandomR r $ FreeT . return . Pure
