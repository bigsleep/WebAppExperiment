{-# LANGUAGE TypeOperators, FlexibleContexts, ExistentialQuantification #-}
module Whone.JsonApi
( JsonApi(..)
, jsonInput
, jsonOutput
, HttpError(..)
) where

import Whone.Internal

import Control.Monad.Error (Error, noMsg, strMsg)
import Control.Monad.Trans.Free (liftF, FreeT(..), FreeF(..))
import qualified Data.ByteString.Lazy as L (ByteString, empty)
import qualified Data.ByteString.Lazy.Char8 as L (pack)
import qualified Data.Aeson as DA (FromJSON, ToJSON)
import qualified Network.HTTP.Types as HTTP (Status, Header, status404)

data JsonApi a =
    forall i. DA.FromJSON i => JsonInput (i -> a) |
    forall o. DA.ToJSON o => JsonOutput o a

instance Functor JsonApi where
    fmap f (JsonInput g) = JsonInput (f . g)
    fmap f (JsonOutput o a) = JsonOutput o (f a)


jsonInput :: (DA.FromJSON i, Monad m, JsonApi :<: f) => (i -> App f m a) -> App f m a
jsonInput a = App . FreeT . return . Free . inject . JsonInput $ (runApp . a)

jsonOutput :: (DA.ToJSON o, Monad m, JsonApi :<: f) => o -> App f m ()
jsonOutput o = App . liftF . inject $ JsonOutput o ()

data HttpError = HttpError {
    httpErrorStatus :: HTTP.Status,
    httpErrorHeader :: [HTTP.Header],
    httpErrorBody :: L.ByteString
    } deriving (Show, Eq)

instance Error HttpError where
    noMsg = HttpError HTTP.status404 [] L.empty
    strMsg = HttpError HTTP.status404 [] . L.pack
