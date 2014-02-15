{-# LANGUAGE TypeOperators, FlexibleContexts, ExistentialQuantification #-}
module Whone.JsonApi
( JsonApi(..)
, getRequest
, putResponse
) where

import Whone.Internal
import Control.Monad.Free (Free(..))
import Data.Aeson (ToJSON, FromJSON)

data JsonApi a =
    forall i. (FromJSON i) => GetRequest (i -> a) |
    forall o. (ToJSON o) => PutResponse o a

instance Functor JsonApi where
    fmap f (GetRequest g) = GetRequest (f . g)
    fmap f (PutResponse y c) = PutResponse y (f c)

getRequest :: (Functor f, JsonApi :<: f, FromJSON i) => App f i
getRequest = App . inject . GetRequest $ Pure

putResponse :: (Functor f, JsonApi :<: f, ToJSON o) => o -> App f ()
putResponse a = App . inject $ PutResponse a (Pure ())
