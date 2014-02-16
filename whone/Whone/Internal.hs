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

eject :: (Monad m, g :<: f) => FreeT f m a -> m (Maybe (g (FreeT f m a)))
eject m = runFreeT m >>= return . f
    where f (Free x) = eje x
          f (Pure _) = Nothing

newtype App f m a = App {runApp :: FreeT f m a} deriving (Functor, Monad)
