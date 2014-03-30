{-# LANGUAGE TypeOperators, FlexibleContexts #-}
module Example.Root
( root
) where

import Whone.Internal ((:<:), App)
import Whone.JsonApi (JsonApi(..), jsonOutput)

import Data.Aeson ()
import Data.Map (singleton)

root :: (JsonApi :<: f, Monad m) => App f m ()
root = jsonOutput $ singleton "description" "this site is a example for Whone."
