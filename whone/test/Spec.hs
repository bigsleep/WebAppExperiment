{-# LANGUAGE TypeOperators, FlexibleInstances #-}
module Whone.Test where

import Whone.Internal ((:+:), App)
import Whone.WebApi (WebApi, getRequest, putResponse)
import Whone.Error (IError)

import Control.Monad (liftM)
import Control.Monad.Error (catchError)
import Control.Monad.Identity (Identity(..))

type F = WebApi () :+: IError String
type WebApp = App F Identity

main = putStrLn

wapp :: (i -> WebApp o) -> WebApp ()
wapp doSomeThing = (getRequest () >>= doSomeThing >>= putResponse ())
                   `catchError` putErrorResponse

putErrorResponse :: String -> WebApp ()
putErrorResponse = putResponse ()

increment = wapp $ liftM (+)

reverse = wapp $ liftM Prelude.reverse

class (Functor f) => RunApp f where
    runApp :: (f a) -> a
