module Whone.WebApi
( setUp
, getRequest
, putResponse
) where

import Whone.Internal

data WebApi s i o a =
    SetUp s a |
    GetRequest (i -> a) |
    PutResponse o a

instance Functor (WebApi s i o) where
    fmap f (SetUp y c) = SetUp y (f c)
    fmap f (GetRequest g) = GetRequest (f . g)
    fmap f (PutResponse y c) = PutResponse y (f c)

instance Interface (WebApi s i o)

setUp :: (Monad m) => i -> App m ()
setUp = createOutput ((I.) . SetUp)

getRequest :: (Monad m) => App m i
getRequest = createInput (I . GetRequest)

putResponse :: (Monad m) => o -> App m ()
putResponse = createOutput ((I.) . PutResponse)

