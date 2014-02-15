{-# LANGUAGE TypeOperators, FlexibleInstances, ScopedTypeVariables, OverloadedStrings #-}
module Main where

import Whone.Internal ((:+:)(..), (:<:), App(..))
import Whone.JsonApi (JsonApi(..), getRequest, putResponse)
import Whone.Error (IError(..))

import Control.Monad (liftM)
import Control.Monad.Trans (lift)
import Control.Monad.State (put)
import Control.Monad.Reader (ask)
import Control.Monad.Error (catchError, throwError)
import Control.Monad.RWS (RWS(..), runRWS)
import Control.Monad.Free (Free(..))
import qualified Data.Aeson as DA (FromJSON, ToJSON, decode, encode)
import qualified Data.ByteString.Lazy as L (ByteString, readFile)

import Test.Hspec
import Test.Hspec.QuickCheck
import qualified Test.QuickCheck.Property as P

main :: IO ()
main = hspec $ jsonApiSpec >> throwErrorApiSpec

-- テスト
jsonApiSpec :: Spec
jsonApiSpec = prop "入力をそのまま返すAPI" $
    \(i :: [Int]) ->
        let s = DA.encode i
            (_, r, _) = runRWS (run' jsonApi1) s ""
            message = "result: " ++ show r ++ " expected: " ++ show s
            result = if r == s
                        then P.succeeded
                        else P.failed {P.reason = message}
        in result

throwErrorApiSpec :: Spec
throwErrorApiSpec = prop "入力が空の場合にエラーになるテスト" $
    \(i :: [Int]) ->
        let s = DA.encode i
            (_, r, _) = runRWS (run' maybeThrowErrorApi1) s ""
            expected = if i == [] then DA.encode ("error: input should be greater 0" :: String) else s
            message = "result: " ++ show r ++ " expected: " ++ show expected
            result = if r == expected
                        then P.succeeded
                        else P.failed {P.reason = message}
        in result

-- テスト用API
type F = JsonApi :+: IError String
type JsonApp = App F

wapp :: (DA.FromJSON i, DA.ToJSON o) => (i -> JsonApp o) -> JsonApp ()
wapp doSomeThing = (getRequest >>= doSomeThing >>= putResponse)
                   `catchError` putErrorResponse

putErrorResponse :: String -> JsonApp ()
putErrorResponse = putResponse

jsonApi1 :: JsonApp ()
jsonApi1 = wapp (return . (id :: [Int] -> [Int]))

maybeThrowErrorApi1 :: JsonApp ()
maybeThrowErrorApi1 = wapp f
    where f :: [Int] -> JsonApp [Int]
          f i = if i == [] then throwError ("error: input should be greater 0" :: String)
                           else return i

-- プログラム実行用関数
type Mock = RWS L.ByteString () L.ByteString ()

run' :: JsonApp a -> Mock
run' (App (Free a)) = run a
run' (App (Pure a)) = return ()

class (Functor f) => Run f where
    run :: f (Free F a) -> Mock

instance (Run f, Run g) => Run (f :+: g) where
    run (Inl a) = run a
    run (Inr a) = run a

instance Run JsonApi where
    run (GetRequest f) = ask >>= \s ->
                         case DA.decode s of
                              Just i -> run' . App . f $ i
                              Nothing -> run (ThrowError ("parse error: " ++ show s :: String))
    run (PutResponse o c) = put (DA.encode o) >> run' (App c)

instance Run (IError String) where
    run (ThrowError s) = put (DA.encode s) >> return ()

