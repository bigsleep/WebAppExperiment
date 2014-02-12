module Whone.Client
( putRequest
, getResponse
) where

import Whone.Internal

data IClient i o a =
    PutRequest i a |
    GetResponse (o -> a)

instance Functor (IClient i o) where
    fmap f (PutRequest y c) = PutRequest y (f c)
    fmap f (GetResponse g) = GetResponse (f . g)

instance Interface (IClient i o)

putRequest :: (Monad m) => i -> App m ()
putRequest = createOutput ((I.) . PutRequest)

getResponse :: (Monad m) => App m o
getResponse = createInput (I . GetResponse)

