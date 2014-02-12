module Whone.Server
( getRequest
, putResponse
) where

import Whone.Internal

data IServer i o a =
    GetRequest (i -> a) |
    PutResponse o a

instance Functor (IServer i o) where
    fmap f (GetRequest g) = GetRequest (f . g)
    fmap f (PutResponse y c) = PutResponse y (f c)

instance Interface (IServer i o)

getRequest :: (Monad m) => App m i
getRequest = createInput (I . GetRequest)

putResponse :: (Monad m) => o -> App m ()
putResponse = createOutput ((I.) . PutResponse)

