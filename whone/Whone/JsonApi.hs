{-# LANGUAGE TypeOperators, FlexibleContexts, ExistentialQuantification #-}
module Whone.JsonApi
( JsonApi(..)
, jsonApi
) where

import Whone.Internal
import Control.Monad.Trans.Free (liftF)
import qualified Data.Aeson as DA (FromJSON, ToJSON)

data JsonApi m a = forall i o. (DA.FromJSON i, DA.ToJSON o) =>JsonApi (i -> m o) a

instance Functor (JsonApi m) where
    fmap f (JsonApi b a) = JsonApi b (f a)

jsonApi :: (DA.FromJSON i, DA.ToJSON o, Monad m, JsonApi n :<: f) => (i -> n o) -> App f m ()
jsonApi api = App . liftF . inject . JsonApi api $ ()
