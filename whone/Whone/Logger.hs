{-# LANGUAGE TypeOperators, FlexibleContexts, FlexibleInstances, TypeFamilies #-}
module Whone.Logger
( ILogger(..)
, LogLevel(..)
, getCurrentLogLevel
, log
, logDebug
, logInfo
, logWarning
, logError
, OutputType
) where

import Whone.Internal
import Prelude hiding (log)
import Control.Monad (when)
import Control.Monad.Trans.Free (FreeT(..), FreeF(..))

data LogLevel = DEBUG | INFO | WARNING | ERROR deriving (Show, Read, Eq, Ord, Enum)

type family OutputType a :: *

data ILogger logger a =
    GetCurrentLogLevel logger (LogLevel -> a) |
    Log logger LogLevel (OutputType logger) a

instance Functor (ILogger logger) where
    fmap f (GetCurrentLogLevel l g) = GetCurrentLogLevel l (f . g)
    fmap f (Log lg lv s a) = Log lg lv s (f a)

getCurrentLogLevel :: (Functor f, Monad m, ILogger logger :<: f) => logger -> App f m LogLevel
getCurrentLogLevel logger = App . inject $ GetCurrentLogLevel logger $ FreeT . return . Pure

log :: (Monad m, ILogger logger :<: f) => LogLevel -> logger -> OutputType logger -> App f m ()
log level logger contents = do
    lv <- getCurrentLogLevel logger
    when (level >= lv) $
         App . inject $ Log logger level contents (FreeT . return . Pure $ ())

logDebug, logInfo, logWarning, logError :: (Monad m, ILogger logger :<: f) => logger -> OutputType logger -> App f m ()
logDebug = log DEBUG
logInfo = log INFO
logWarning = log WARNING
logError = log ERROR
