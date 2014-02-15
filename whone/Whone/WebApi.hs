{-# LANGUAGE TypeOperators, FlexibleContexts, ExistentialQuantification #-}
module Whone.WebApi
( WebApi
, getRequest
, putResponse
) where

import Whone.Internal
import Control.Monad.Trans.Free (FreeT(..), FreeF(..))

data WebApi s a =
    forall i. GetRequest s (i -> a) |
    forall o. PutResponse s o a

instance Functor (WebApi s) where
    fmap f (GetRequest s g) = GetRequest s (f . g)
    fmap f (PutResponse s y c) = PutResponse s y (f c)

getRequest :: (Monad m, WebApi s :<: f) => s -> App f m i
getRequest s = inject (GetRequest s $ FreeT . return . Pure)

putResponse :: (Monad m, WebApi s :<: f) => s -> o -> App f m ()
putResponse s a = inject (PutResponse s a (FreeT . return . Pure $ ()))
