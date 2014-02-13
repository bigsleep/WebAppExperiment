module Whone.WebClient
( setUp
, putRequest
, getResponse
) where

import Whone.Internal

data WebClient s i o a =
    SetUp s a |
    PutRequest i a |
    GetResponse (o -> a)

instance Functor (WebClient s i o) where
    fmap f (SetUp y c) = SetUp y (f c)
    fmap f (PutRequest y c) = PutRequest y (f c)
    fmap f (GetResponse g) = GetResponse (f . g)

instance Interface (WebClient s i o)

setUp :: (Monad m) => i -> App m ()
setUp = createOutput ((I.) . SetUp)

putRequest :: (Monad m) => i -> App m ()
putRequest = createOutput ((I.) . PutRequest)

getResponse :: (Monad m) => App m o
getResponse = createInput (I . GetResponse)

