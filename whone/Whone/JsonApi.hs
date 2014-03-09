{-# LANGUAGE TypeOperators, FlexibleContexts, ExistentialQuantification #-}
module Whone.JsonApi
( JsonApi(..)
, jsonApi
, HttpError(..)
) where

import Whone.Internal

import Control.Monad.Error (Error, noMsg, strMsg)
import Control.Monad.Trans.Free (liftF)
import qualified Data.ByteString.Lazy as L (ByteString, empty)
import qualified Data.ByteString.Lazy.Char8 as L (pack)
import qualified Data.Aeson as DA (FromJSON, ToJSON)
import qualified Network.HTTP.Types as HTTP (Status, Header, status404)

data JsonApi m a = forall i o. (DA.FromJSON i, DA.ToJSON o) =>JsonApi (i -> m o) a

instance Functor (JsonApi m) where
    fmap f (JsonApi b a) = JsonApi b (f a)

jsonApi :: (DA.FromJSON i, DA.ToJSON o, Monad m, JsonApi n :<: f) => (i -> n o) -> App f m ()
jsonApi api = App . liftF . inject $ JsonApi api ()

data HttpError = HttpError {
    httpErrorStatus :: HTTP.Status,
    httpErrorHeader :: [HTTP.Header],
    httpErrorBody :: L.ByteString
    } deriving (Show, Eq)

instance Error HttpError where
    noMsg = HttpError HTTP.status404 [] L.empty
    strMsg = HttpError HTTP.status404 [] . L.pack
