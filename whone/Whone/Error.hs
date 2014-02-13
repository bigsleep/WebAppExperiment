{-# LANGUAGE TypeOperators, MultiParamTypeClasses, FlexibleInstances, UndecidableInstances, FlexibleContexts, ExistentialQuantification, ViewPatterns #-}
module Whone.Error
() where

import Whone.Internal
import Control.Monad.Identity (Identity(..))
--import Control.Monad.Error (Error, MonadError, throwError, catchError)
import Control.Monad.Trans.Free (FreeT(..), FreeF(..), liftF)

data IError e a =
    ThrowError e

instance Functor (IError e) where
    fmap _ (ThrowError e) = ThrowError e

{-
instance (Error e, IError e :<: f) => MonadError e (App f Identity) where
    throwError e = inject (ThrowError e)
    catchError (FreeT (Identity (Free (inject (ThrowError e))))) f = f e
    catchError a _ = a
-}

throwError :: (Monad m, IError e :<: f) => e -> App f m a
throwError = inject . ThrowError

--catchError :: (Monad m, IError e :<: f) => App f m a -> (e -> App f m a) -> App f m a
