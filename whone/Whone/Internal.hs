{-# LANGUAGE TypeOperators, MultiParamTypeClasses, OverlappingInstances, FlexibleInstances, GeneralizedNewtypeDeriving, RankNTypes #-}
module Whone.Internal
( (:+:)(..)
, (:<:)
, inject
, eject
, inj
, eje
, App(..)
) where

import Control.Monad.Free (Free(..))

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

inject :: (g :<: f) => g (Free f a) -> Free f a
inject = Free . inj

eject :: (g :<: f) => Free f a -> Maybe (g (Free f a))
eject (Free a) = eje a
eject (Pure _) = Nothing

newtype App f a = App {runApp :: Free f a} deriving (Functor, Monad)
