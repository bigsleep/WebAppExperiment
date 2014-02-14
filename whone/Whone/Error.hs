{-# LANGUAGE TypeOperators, MultiParamTypeClasses, FlexibleInstances, UndecidableInstances, FlexibleContexts #-}
module Whone.Error
( IError(..)
) where

import Whone.Internal
import Control.Monad.Identity (Identity(..))
import Control.Monad.Error (Error, MonadError, throwError, catchError)

data IError e a =
    ThrowError e

instance Functor (IError e) where
    fmap _ (ThrowError e) = ThrowError e

instance (Error e, IError e :<: f) => MonadError e (App f Identity) where
    throwError e = inject (ThrowError e)
    catchError a f = case eject a of
                          Just (ThrowError e) -> f e
                          _ -> a
