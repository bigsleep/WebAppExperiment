{-# LANGUAGE TypeOperators, FlexibleContexts, ExistentialQuantification #-}
module Whone.WebClient
( putRequest
, getResponse
) where

import Whone.Internal
import Control.Monad.Free (Free(..))

data WebClient s a =
    forall i. PutRequest s i a |
    forall o. GetResponse s (o -> a)

instance Functor (WebClient s) where
    fmap f (PutRequest s y c) = PutRequest s y (f c)
    fmap f (GetResponse s g) = GetResponse s (f . g)

putRequest :: (WebClient s :<: f) => s -> o -> App f ()
putRequest s a = App . inject $ PutRequest s a (Pure ())

getResponse :: (WebClient s :<: f) => s -> App f i
getResponse s = App . inject . GetResponse s $ Pure
