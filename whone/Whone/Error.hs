{-# LANGUAGE TypeOperators, MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, UndecidableInstances #-}
module Whone.Error
( IError(..)
) where

import Whone.Internal
import Control.Monad.Trans (lift)
import Control.Monad.Error (Error, MonadError, throwError, catchError)

data IError e a =
    ThrowError e

instance Functor (IError e) where
    fmap _ (ThrowError e) = ThrowError e

instance (Monad m, Error e, IError e :<: f) => MonadError e (App f m) where
    throwError e = App . inject $ (ThrowError e)
    catchError a f = do
        r <- App . lift . eject . runApp $ a
        case r of
            Just (ThrowError e) -> f e
            _ -> a

