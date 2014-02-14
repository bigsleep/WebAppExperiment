{-# LANGUAGE TypeOperators, MultiParamTypeClasses, OverlappingInstances, FlexibleInstances #-}
module Whone.Internal
( (:+:)
, (:<:)
, inject
, eject
, App
) where

import Control.Monad.Identity (Identity(..))
import Control.Monad.Trans.Free (FreeT(..), FreeF(..))

data (f :+: g) e = Inl (f e) | Inr (g e)
infixr 6 :+:

class (Functor sub, Functor sup) => sub :<: sup where
    inj :: sub a -> sup a
    eje :: sup a -> Maybe (sub a)

instance (Functor f, Functor g) => Functor (f :+: g) where
    fmap f (Inl e1)  = Inl (fmap f e1)
    fmap f (Inr e2)  = Inr (fmap f e2)

instance Functor f => f :<: f where
    inj = id
    eje = Just

instance (Functor f, Functor g) => f :<: (f :+: g) where
    inj = Inl
    eje (Inl a) = Just a
    eje _ = Nothing

instance (Functor f, Functor g, Functor h, f :<: g) => f :<: (h :+: g) where
    inj = Inr . inj
    eje (Inr a) = eje a
    eje _ = Nothing

inject :: (Monad m, g :<: f) => g (FreeT f m a) -> FreeT f m a
inject = FreeT . return . Free . inj

eject :: (g :<: f) => FreeT f Identity a -> Maybe (g (FreeT f Identity a))
eject (FreeT (Identity (Free a))) = eje a

type App = FreeT
