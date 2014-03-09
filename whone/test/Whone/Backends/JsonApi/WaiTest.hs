{-# LANGUAGE RankNTypes, FlexibleContexts, OverloadedStrings #-}
module Whone.Backends.JsonApi.WaiTest
( run
) where

import Whone.JsonApi (JsonApi(..), HttpError(..))

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, ask)
import Control.Monad.State (MonadState, put)
import Control.Monad.Error (MonadError, catchError)
import qualified Data.Aeson as DA (FromJSON, ToJSON, decode, encode)
import qualified Data.ByteString.Lazy as L (fromChunks)

import Network.HTTP.Types (status200, status400)
import qualified Network.Wai as W (Application, Request, requestBody, responseLBS)
import qualified Network.Wai.Test as WT (runSession, request, SResponse)
import qualified Data.Conduit as C (($$))
import qualified Data.Conduit.List as CL (consume)

run :: (Monad m, MonadIO m, MonadReader W.Request m, MonadState WT.SResponse m, MonadError HttpError m)
    => (forall a. n a -> m a) -> (forall b. m b -> IO b) -> JsonApi n (m c) -> m c
run toM toIO (JsonApi app c) = do
    req <- ask
    res <- liftIO $ WT.runSession (WT.request req) (toApplication toIO (toM . app))
    put res
    c

toApplication :: (DA.FromJSON i, DA.ToJSON o, Monad m, MonadIO m, MonadError HttpError m)
              => (forall b. m b -> IO b) -> (i -> m o) -> W.Application
toApplication toIO app = \request -> W.requestBody request C.$$ sink
    where sink = do
                 i <- CL.consume
                 case DA.decode . L.fromChunks $ i of
                      Just input -> liftIO . toIO $ (app input >>= putResponse) `catchError` putErrorResponse
                      Nothing -> badRequest
          putResponse = return . W.responseLBS status200 [] . DA.encode
          putErrorResponse (HttpError s h b) = return $ W.responseLBS s h b
          badRequest = return $ W.responseLBS status400 [] "error"

