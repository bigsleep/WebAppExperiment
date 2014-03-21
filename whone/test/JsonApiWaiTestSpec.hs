{-# LANGUAGE TypeOperators, MultiParamTypeClasses, FlexibleInstances, ScopedTypeVariables, OverloadedStrings #-}
module JsonApiWaiTestSpec
( jsonApiWaiTestSpec
) where

import Whone.Internal ((:+:)(..), App(..))
import Whone.JsonApi (JsonApi(..), jsonApi, HttpError(..))
import Whone.Error (IError(..))

import qualified Whone.Backends.JsonApi.WaiTest as BWT (run)

import Control.Monad (liftM)
import Control.Monad.Trans (lift)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ask)
import Control.Monad.Error (throwError, strMsg)
import Control.Monad.RWS (RWST(..), runRWST)
import Control.Monad.Trans.Free (iterT)
import qualified Data.Aeson as DA (decode, encode)
import qualified Data.ByteString.Lazy as L (ByteString, empty, toStrict, fromChunks)
import qualified Data.Conduit.List as C (sourceList, consume)

import Test.Hspec
import Test.Hspec.QuickCheck
import qualified Test.QuickCheck.Property as P
import qualified Network.Wai as W (Request, responseLBS, defaultRequest, requestMethod, requestBody, lazyRequestBody, rawPathInfo)
import qualified Network.Wai.Test as WT (SRequest(..), SResponse(..))
import qualified Network.HTTP.Types as HTTP (status200, status400, status404, methodPost)

jsonApiWaiTestSpec :: Spec
jsonApiWaiTestSpec = simpleApiSpec >> throwErrorApiSpec

-- テスト
simpleApiSpec :: Spec
simpleApiSpec  = prop "入力をそのまま返すAPI" $
    \(i :: [Int]) -> P.morallyDubiousIOProperty $ do
        let s = DA.encode i
        let req = WT.SRequest W.defaultRequest s
        let expected = WT.SResponse HTTP.status200 [] s
        (_, r, _) <- runRWST (run' jsonApi1) req defaultResponse
        let message = "result: " ++ show r ++ " expected: " ++ show expected
        let result = if r == expected
                        then P.succeeded
                        else P.failed {P.reason = message}
        return result
    where defaultResponse = WT.SResponse HTTP.status404 [] L.empty

throwErrorApiSpec :: Spec
throwErrorApiSpec = prop "入力が空の場合にエラーになるテスト" $
    \(i :: [Int]) -> P.morallyDubiousIOProperty $ do
        let s = DA.encode i
        let req = WT.SRequest W.defaultRequest s
        (_, r, _) <- runRWST (run' maybeThrowErrorApi1) req defaultResponse
        let expected = if i == []
                          then WT.SResponse HTTP.status400 [] "error: input size should be greater 0"
                          else WT.SResponse HTTP.status200 [] s
        let message = "result: " ++ show r ++ " expected: " ++ show expected
        let result = if r == expected
                        then P.succeeded
                        else P.failed {P.reason = message}
        return result
    where defaultResponse = WT.SResponse HTTP.status404 [] L.empty

type F = JsonApi N :+: IError HttpError
newtype N a = N { runN :: App F Mock a }
type JsonApp = App F Mock

jsonApi1 :: JsonApp ()
jsonApi1 = jsonApi (N . return . id :: [Int] -> N [Int])

maybeThrowErrorApi1 :: JsonApp ()
maybeThrowErrorApi1 = jsonApi f
    where f :: [Int] -> N [Int]
          f i = if i == [] then N . throwError $ HttpError HTTP.status400 [] "error: input size should be greater 0"
                           else N . return $ i

-- プログラム実行用関数
type Mock = RWST WT.SRequest () WT.SResponse IO

run' :: JsonApp a -> Mock a
run' a = iterT run (runApp a)

class (Functor f) => Run f where
    run :: f (Mock a) -> Mock a

instance (Run f, Run g) => Run (f :+: g) where
    run (Inl a) = run a
    run (Inr a) = run a

instance Run (JsonApi N) where
    run (JsonApi app c) = do
        r <- ask
        BWT.run run' (toIO r) (JsonApi (runN . app) c)
        where toIO :: WT.SRequest -> Mock a -> IO a
              toIO r m = runRWST m r defaultResponse >>= \(a, _, _) -> return a
              defaultResponse = WT.SResponse HTTP.status404 [] L.empty

instance Run (IError HttpError) where
    run (ThrowError e) = undefined
