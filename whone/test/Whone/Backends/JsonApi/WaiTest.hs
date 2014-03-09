{-# LANGUAGE RankNTypes, FlexibleContexts, OverloadedStrings #-}
module Whone.Backends.JsonApi.WaiTest
( run
) where

import Whone.JsonApi (JsonApi(..), HttpError(..))

import Control.Monad (liftM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, ask)
import Control.Monad.State (MonadState, put)
import Control.Monad.Error (MonadError, catchError)
import qualified Data.Aeson as DA (FromJSON, ToJSON, decode, encode)
import qualified Data.ByteString.Lazy as L (fromChunks)

import Network.HTTP.Types (status200, status404)
import qualified Network.Wai as W (Application, Request, requestBody, responseLBS)
import qualified Network.Wai.Test as WT (runSession, srequest, SRequest, SResponse)
import qualified Data.Conduit as C (($$))
import qualified Data.Conduit.List as CL (consume)

run :: (Monad m, MonadIO m, MonadReader WT.SRequest m, MonadState WT.SResponse m, MonadError HttpError n)
    => (forall a. n a -> m a) -> (forall a. m a -> IO a) -> JsonApi n (m b) -> m b

run toM toIO (JsonApi app c) = do
    req <- ask
    res <- liftIO $ WT.runSession (WT.srequest req) (toApplication (toIO . toM) app)
    put res
    c

toApplication :: (DA.FromJSON i, DA.ToJSON o, MonadError HttpError m)
              => (forall b. m b -> IO b) -> (i -> m o) -> W.Application
toApplication toIO app = \request -> W.requestBody request C.$$ sink
    where sink = do
                 i <- liftM L.fromChunks CL.consume
                 case DA.decode i of
                      Just input -> liftIO . toIO $ (app input >>= putResponse) `catchError` putErrorResponse
                      Nothing -> badRequest
          putResponse = return . W.responseLBS status200 [] . DA.encode
          putErrorResponse (HttpError s h b) = return $ W.responseLBS s h b
          badRequest = return $ W.responseLBS status404 [] "error"

