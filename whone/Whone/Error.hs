{-# LANGUAGE TypeOperators, MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, UndecidableInstances #-}
module Whone.Error
( IError(..)
) where

import Whone.Internal
import Control.Monad.Error (Error, MonadError, throwError, catchError)

data IError e a =
    ThrowError e

instance Functor (IError e) where
    fmap _ (ThrowError e) = ThrowError e

instance (Functor f, Error e, IError e :<: f) => MonadError e (App f) where
    throwError e = App . inject $ (ThrowError e)
    catchError a f = case eject (runApp a) of
                          Just (ThrowError e) -> f e
                          _ -> a

