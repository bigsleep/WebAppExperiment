{-# LANGUAGE TypeOperators, FlexibleInstances #-}
module Main where

import Whone.Internal ((:+:)(..), (:<:), App)
import Whone.JsonApi (JsonApi(..), getRequest, putResponse)
import Whone.Error (IError(..))

import Control.Monad (liftM)
import Control.Monad.Trans (lift)
import Control.Monad.Error (catchError)
import Control.Monad.Reader (ReaderT(..), ask)
import Control.Monad.Free (Free(..))
import qualified Data.Aeson as DA (FromJSON, ToJSON, decode)
import qualified Data.ByteString.Lazy as L (ByteString, readFile)

data S i o = S
type F = JsonApi :+: IError String
type WebApp = App F

main :: IO ()
main = putStrLn "test"

wapp :: (DA.FromJSON i, DA.ToJSON o) => (i -> WebApp o) -> WebApp ()
wapp doSomeThing = (getRequest >>= doSomeThing >>= putResponse)
                   `catchError` putErrorResponse

putErrorResponse :: String -> WebApp ()
putErrorResponse = putResponse

type Mock = ReaderT L.ByteString Maybe

class (Functor f) => Run f where
    run :: f (Free F a) -> Mock a

instance (Run f, Run g) => Run (f :+: g) where
    run (Inl a) = run a
    run (Inr a) = run a

run' :: Free F a -> Mock a
run' (Free a) = run a
run' (Pure a) = return a

instance Run JsonApi where
    run (GetRequest f) = ask >>= lift . DA.decode >>= run' . f
    run (PutResponse _ c) = run' c

instance Run (IError String) where
    run (ThrowError _) = lift Nothing
