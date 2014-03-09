{-# LANGUAGE TypeOperators, MultiParamTypeClasses, FlexibleInstances, ScopedTypeVariables, OverloadedStrings #-}
module JsonApiSpec
( jsonApiSpec
) where

import Whone.Internal ((:+:)(..), App(..))
import Whone.JsonApi (JsonApi(..), jsonApi)
import Whone.Error (IError(..))

import Control.Monad.Trans (lift)
import Control.Monad.Reader (ask)
import Control.Monad.Error (throwError)
import Control.Monad.RWS (RWST(..), runRWST)
import Control.Monad.Trans.Free (iterT)
import qualified Data.Aeson as DA (decode, encode)
import qualified Data.ByteString.Lazy as L (ByteString)

import Test.Hspec
import Test.Hspec.QuickCheck
import qualified Test.QuickCheck.Property as P

jsonApiSpec :: Spec
jsonApiSpec = simpleApiSpec >> throwErrorApiSpec

-- テスト
simpleApiSpec :: Spec
simpleApiSpec  = prop "入力をそのまま返すAPI" $
    \(i :: [Int]) ->
        let s = DA.encode i
            Left r = runRWST (run' jsonApi1) s ()
            message = "result: " ++ show r ++ " expected: " ++ show s
            result = if r == s
                        then P.succeeded
                        else P.failed {P.reason = message}
        in result

throwErrorApiSpec :: Spec
throwErrorApiSpec = prop "入力が空の場合にエラーになるテスト" $
    \(i :: [Int]) ->
        let s = DA.encode i
            Left r = runRWST (run' maybeThrowErrorApi1) s ()
            expected = if i == [] then DA.encode ("error: input should be greater 0" :: String) else s
            message = "result: " ++ show r ++ " expected: " ++ show expected
            result = if r == expected
                        then P.succeeded
                        else P.failed {P.reason = message}
        in result

-- テスト用API
type F = JsonApi N :+: IError String
newtype N a = N { runN :: App F Mock a }
type JsonApp = App F Mock

jsonApi1 :: JsonApp ()
jsonApi1 = jsonApi (N . return . id :: [Int] -> N [Int])

maybeThrowErrorApi1 :: JsonApp ()
maybeThrowErrorApi1 = jsonApi f
    where f :: [Int] -> N [Int]
          f i = if i == [] then N . throwError $ ("error: input should be greater 0" :: String)
                           else N . return $ i
-- プログラム実行用関数
type Mock = RWST L.ByteString () () (Either L.ByteString)

run' :: JsonApp a -> Mock a
run' a = iterT run (runApp a)

class (Functor f) => Run f where
    run :: f (Mock a) -> Mock a

instance (Run f, Run g) => Run (f :+: g) where
    run (Inl a) = run a
    run (Inr a) = run a

instance Run (JsonApi N) where
    run (JsonApi f c) = do
        s <- ask
        case DA.decode s of
             Just i -> (run' . runN . f $ i) >>= lift . Left . DA.encode >> c
             _ -> run (ThrowError ("parse error: " ++ show s :: String))

instance Run (IError String) where
    run (ThrowError s) = lift . Left . DA.encode $ s
