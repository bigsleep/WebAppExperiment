{-# LANGUAGE ExistentialQuantification #-}
module Whone.Internal
( App
, Interface
, I(..)
, createInput
, createOutput
) where

import Control.Monad.Trans.Free (FreeT(..), FreeF(..))

class (Functor f) => Interface f

data I a = forall f. (Interface f) => I (f a)

instance Functor I where
    fmap f (I a) = I $ fmap f a

type App = FreeT I

createInput :: (Functor f, Monad m) => ((a -> FreeT f m a) -> f (FreeT f m a)) -> FreeT f m a
createInput f = FreeT . return . Free . f $ FreeT . return . Pure

createOutput :: (Functor f, Monad m) => (a -> FreeT f m () -> f (FreeT f m ())) -> a -> FreeT f m ()
createOutput f x = FreeT . return . Free $ f x (FreeT . return . Pure $ ())

