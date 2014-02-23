{-# LANGUAGE TypeOperators, MultiParamTypeClasses, FlexibleContexts, TypeFamilies #-}
module Whone.Backends.Logger.Mock
( run
) where

import Whone.Logger (ILogger(..), LogLevel(..), OutputType)
import Control.Monad.Writer (MonadWriter, tell)

run :: (Monad m, MonadWriter w m, OutputType logger ~ w) => LogLevel -> ILogger logger (m a) -> m a
run l (Log _ lv s a) = if lv >= l then tell s >> a else a
