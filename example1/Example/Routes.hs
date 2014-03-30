{-# LANGUAGE TypeOperators, FlexibleContexts, FlexibleInstances, QuasiQuotes, OverloadedStrings, TypeFamilies #-}
module Example.Routes
( example
) where

import Example.Root (root)
import Run (F, M)

import Whone.Internal (App)
import Whone.Routes (parseRoutes)

example :: App F M ()
example = [parseRoutes|
GET     /           root'
|]

root' :: App F M ()
root' = root
