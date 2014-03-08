{-# LANGUAGE TypeOperators, FlexibleContexts, TypeFamilies #-}
module Whone.JsonApi
( JsonApi(..)
, jsonApi
) where

import Whone.Internal
import Control.Monad.Trans.Free (FreeT(..), FreeF(..))

data JsonApi i o m a = JsonApi (i -> m o) a

instance Functor (JsonApi i o m) where
    fmap f (JsonApi b a) = JsonApi b (f a)

jsonApi :: (Monad m, JsonApi i o n :<: f) => (i -> n o) -> App f m ()
jsonApi api = App . inject . JsonApi api $ (FreeT . return . Pure $ ())
