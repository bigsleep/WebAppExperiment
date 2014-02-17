{-# LANGUAGE TypeOperators, FlexibleContexts #-}
module Whone.Api
( Signature(..)
, Api(..)
, getRequest
, putResponse
) where

import Whone.Internal
import Control.Monad.Trans.Free (FreeT(..), FreeF(..))

data Signature i o = Signature

data Api i o a =
    GetRequest (Signature i o) (i -> a) |
    PutResponse (Signature i o) o a

instance Functor (Api i o) where
    fmap f (GetRequest s g) = GetRequest s (f . g)
    fmap f (PutResponse s y c) = PutResponse s y (f c)

getRequest :: (Monad m, Api i o :<: f) => Signature i o -> App f m i
getRequest s = App . inject . GetRequest s $ (FreeT . return . Pure)

putResponse :: (Monad m, Api i o :<: f) => Signature i o -> o -> App f m ()
putResponse s a = App . inject . PutResponse s a $ (FreeT . return . Pure $ ())

