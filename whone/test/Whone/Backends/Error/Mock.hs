{-# LANGUAGE TypeOperators, MultiParamTypeClasses, FlexibleContexts #-}
module Whone.Backends.Error.Mock
( run
) where

import Whone.Error (IError(..))

run :: (Error e) => (e -> m a) -> IError e (m a) -> m a
run onError (ThrowError e) = onError e
