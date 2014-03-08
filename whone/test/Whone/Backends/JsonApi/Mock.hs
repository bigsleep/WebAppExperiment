{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, RankNTypes #-}
module Whone.Backends.JsonApi.Mock
( run
) where

import Whone.JsonApi (JsonApi(..))
import Control.Monad.Reader (MonadReader, ask)
import Control.Monad.State (MonadState, put)
import qualified Data.Aeson as DA (decode, encode)
import qualified Data.ByteString.Lazy as L (ByteString)

run :: (MonadReader L.ByteString m, MonadState L.ByteString m) =>
       (String -> m a) -> (forall b. n b -> m b) -> JsonApi n (m a) -> m a

run onError run' (JsonApi f c) = do
    s <- ask
    case DA.decode s of
         Just i -> run' (f i) >>= put . DA.encode >> c
         Nothing -> onError $ "parse error: " ++ show s
