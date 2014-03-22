module Whone.Backends.Error.Mock
( run
) where

import Whone.Error (IError(..))
import Control.Monad.Error (Error)

run :: (Error e) => (e -> m a) -> IError e (m a) -> m a
run onError (ThrowError e) = onError e
