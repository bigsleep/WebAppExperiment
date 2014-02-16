{-# LANGUAGE TypeOperators, FlexibleContexts, ExistentialQuantification #-}
module Whone.WebClient
( putRequest
, getResponse
) where

import Whone.Internal
import Control.Monad.Trans.Free (FreeT(..), FreeF(..))

data WebClient s a =
    forall i. PutRequest s i a |
    forall o. GetResponse s (o -> a)

instance Functor (WebClient s) where
    fmap f (PutRequest s y c) = PutRequest s y (f c)
    fmap f (GetResponse s g) = GetResponse s (f . g)

putRequest :: (Monad m, WebClient s :<: f) => s -> o -> App f m ()
putRequest s a = App . inject $ PutRequest s a (FreeT . return . Pure $ ())

getResponse :: (Monad m, WebClient s :<: f) => s -> App f m i
getResponse s = App . inject . GetResponse s $ FreeT . return . Pure
