{-# LANGUAGE TypeOperators, FlexibleContexts, ExistentialQuantification #-}
module Whone.JsonApi
( JsonApi(..)
, getRequest
, putResponse
) where

import Whone.Internal
import Control.Monad.Trans.Free (FreeT(..), FreeF(..))
import Data.Aeson (ToJSON, FromJSON)

data JsonApi a =
    forall i. (FromJSON i) => GetRequest (i -> a) |
    forall o. (ToJSON o) => PutResponse o a

instance Functor JsonApi where
    fmap f (GetRequest g) = GetRequest (f . g)
    fmap f (PutResponse y c) = PutResponse y (f c)

getRequest :: (Monad m, JsonApi :<: f, FromJSON i) => App f m i
getRequest = App . inject . GetRequest $ FreeT . return . Pure

putResponse :: (Monad m, JsonApi :<: f, ToJSON o) => o -> App f m ()
putResponse a = App . inject $ PutResponse a (FreeT . return . Pure $ ())
