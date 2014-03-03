{-# LANGUAGE TypeOperators, MultiParamTypeClasses, FlexibleContexts, TypeFamilies #-}
module Whone.Backends.Logger.Mock
( run
) where

import Whone.Logger (ILogger(..), LogLevel(..), OutputType)
import Control.Monad.State (MonadState, get, put)

run :: (Monad m, MonadState s m, [(LogLevel, OutputType logger)] ~ s) => LogLevel -> ILogger logger (m a) -> m a

run _ (Log _ lv s a) = get >>= \x -> put (x ++ [(lv, s)]) >> a

run l (GetCurrentLogLevel _ g) = g l
