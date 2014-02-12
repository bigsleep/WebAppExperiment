module Whone.Error
( putError
) where

import Whone.Internal
import Control.Monad.Trans.Free (liftF)

data IError e a =
    PutError e

instance Functor (IError e) where
    fmap _ (PutError e) = PutError e

instance Interface (IError e)

putError :: (Monad m) => e -> App m ()
putError = liftF . I . PutError
