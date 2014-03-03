{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts #-}
module Whone.Backends.JsonApi.Mock
( run
) where

import Whone.JsonApi (JsonApi(..))
import Control.Monad.Reader (MonadReader, ask)
import Control.Monad.State (MonadState, put)
import qualified Data.Aeson as DA (decode, encode)
import qualified Data.ByteString.Lazy as L (ByteString)

run :: (MonadReader L.ByteString m, MonadState L.ByteString m) => (String -> m a) -> JsonApi (m a) -> m a

run onError (GetRequest f) = ask >>= \s ->
                             case DA.decode s of
                                  Just i -> f i
                                  Nothing -> onError $ "parse error: " ++ show s

run _ (PutResponse y c) = put (DA.encode y) >> c
