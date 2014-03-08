{-# LANGUAGE TypeOperators, MultiParamTypeClasses, FlexibleInstances, ScopedTypeVariables, IncoherentInstances, OverloadedStrings, GeneralizedNewtypeDeriving #-}
module JsonApiSpec
( jsonApiSpec
) where

import Whone.Internal ((:+:)(..), (:<:), App(..), inj, eje)
import Whone.JsonApi (JsonApi(..), jsonApi)
import Whone.Error (IError(..))

import Control.Monad.Trans (lift)
import Control.Monad.State (put)
import Control.Monad.Reader (ask)
import Control.Monad.Error (catchError, throwError)
import Control.Monad.RWS (RWST(..), runRWST)
import Control.Monad.Trans.Free (FreeT(..), FreeF(..), iterT)
import qualified Data.Aeson as DA (FromJSON, ToJSON, decode, encode)
import qualified Data.ByteString.Lazy as L (ByteString, readFile)

import Debug.Trace (trace)
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
type F i o = JsonApi i o (N i o) :+: IError String
newtype N i o a = N { runN :: App (F i o) Mock a }
type JsonApp i o = App (F i o) Mock

jsonApi1 :: JsonApp [Int] [Int] ()
jsonApi1 = jsonApi (N . return . id :: [Int] -> N [Int] [Int] [Int])

maybeThrowErrorApi1 :: JsonApp [Int] [Int] ()
maybeThrowErrorApi1 = jsonApi f
    where f :: [Int] -> N [Int] [Int] [Int]
          f i = if i == [] then N . throwError $ ("error: input should be greater 0" :: String)
                           else N . return $ i
-- プログラム実行用関数
type Mock = RWST L.ByteString () () (Either L.ByteString)

run' :: (DA.FromJSON i, DA.ToJSON o) => JsonApp i o a -> Mock a
run' a = iterT run (runApp a)

class (Functor f) => Run f where
    run :: f (Mock a) -> Mock a

instance (Run f, Run g) => Run (f :+: g) where
    run (Inl a) = run a
    run (Inr a) = run a

instance (DA.FromJSON i, DA.ToJSON o) => Run (JsonApi i o (N i o)) where
    run (JsonApi f c) = do
        s <- ask
        case DA.decode s of
             Just i -> (run' . runN . f $ i) >>= lift .Left . DA.encode >> c
             _ -> run (ThrowError ("parse error: " ++ show s :: String))

instance Run (IError String) where
    run (ThrowError s) = lift . Left . DA.encode $ s
