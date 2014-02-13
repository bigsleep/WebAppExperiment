{-# LANGUAGE TypeOperators, MultiParamTypeClasses, OverlappingInstances, FlexibleInstances #-}
module Whone.Internal
( (:+:)
, (:<:)
, inject
, App
) where

import Control.Monad.Trans.Free (FreeT(..), FreeF(..))

data (f :+: g) e = Inl (f e) | Inr (g e)
infixr 6 :+:

class (Functor sub, Functor sup) => (:<:) sub sup where
    inj :: sub a -> sup a

instance (Functor f, Functor g) => Functor (f :+: g) where
    fmap f (Inl e1)  = Inl (fmap f e1)
    fmap f (Inr e2)  = Inr (fmap f e2)

instance Functor f => (:<:) f f where
    inj = id

instance (Functor f, Functor g) => (:<:) f (f :+: g) where
    inj = Inl

instance (Functor f, Functor g, Functor h, (:<:) f g) =>
    (:<:) f (h :+: g) where
        inj = Inr . inj

inject :: (Monad m, Functor f, Functor g, (:<:) g f) => g (FreeT f m a) -> FreeT f m a
inject = FreeT . return . Free . inj

type App = FreeT
