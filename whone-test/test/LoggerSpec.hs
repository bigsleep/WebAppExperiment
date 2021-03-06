{-# LANGUAGE TypeOperators, FlexibleInstances, TypeFamilies, ScopedTypeVariables, OverloadedStrings #-}
module LoggerSpec
( loggerSpec
) where

import Whone.Internal ((:+:)(..), App(..))
import qualified Whone.Logger as L (ILogger(..), LogLevel(..), OutputType, log, logDebug, logInfo, logWarning, logError)
import Whone.JsonApi (JsonApi(..), jsonInput, jsonOutput)
import Whone.Error (IError(..))

import qualified Whone.Backends.Logger.Mock as LM (run)
import qualified Whone.Backends.Error.Mock as EM (run)

import Control.Monad.Error (throwError)
import Control.Monad.Reader (ask)
import Control.Monad.State (get)
import Control.Monad.RWS (RWST(..))
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Free (iterT)
import qualified Data.ByteString.Lazy as BS (ByteString, drop)
import qualified Data.Aeson as DA (decode, encode)

import Test.Hspec
import Test.Hspec.QuickCheck
import qualified Test.QuickCheck.Property as P
import qualified Test.QuickCheck as Q (Arbitrary, arbitrary, elements)

type M = RWST BS.ByteString () [(L.LogLevel, String)] (Either (String, [(L.LogLevel, String)]))
type F = L.ILogger () :+: JsonApi :+: IError String
type MyApp = App F M

loggerSpec :: Spec
loggerSpec = spec1 >> spec2 >> spec3

-- specs
spec1 :: Spec
spec1 = prop "ログ確認テスト" $
    \(i :: [Int]) ->
        let s = DA.encode i
            expected = [
                (L.INFO, "app1 start"),
                (L.INFO, "request: " ++ show s),
                (L.INFO, "this api just return input"),
                (L.INFO, "response: " ++ show s),
                (L.INFO, "app1 end")]
            Right (_, l, _) = runRWST (run' app1) s []
            message = "result: " ++ show l ++ " expected: " ++ show expected
            result = if l == expected
                        then P.succeeded
                        else P.failed {P.reason = message}
        in result

spec2 :: Spec
spec2 = prop "パースエラー時ログ" $
    \(i :: [Int]) ->
        let s = BS.drop 1 $ DA.encode i
            expected = [
                (L.INFO, "app1 start"),
                (L.ERROR, "parse error: " ++ show s)]
            Left (_, l) = runRWST (run' app1) s []
            message = "result: " ++ show l ++ " expected: " ++ show expected
            result = if l == expected
                        then P.succeeded
                        else P.failed {P.reason = message}
        in result

spec3 :: Spec
spec3 = prop "ログレベル" $
    \((i, s) :: (L.LogLevel, String)) ->
        let app = L.log i () s :: MyApp ()
            Right (_, l, _) = runRWST (run' app) "" []
            expected = if i >= L.INFO then [(i, s)] else []
            message = "result: " ++ show l ++ " expected: " ++ show expected
            result = if l == expected
                        then P.succeeded
                        else P.failed {P.reason = message}
        in result



-- apps
app1' :: [Int] -> MyApp [Int]
app1' i = do
    L.logInfo () ("request: " ++ show (DA.encode i))
    L.logInfo () "this api just return input"
    L.logInfo () ("response: " ++ show (DA.encode i))
    return i

app1 :: MyApp ()
app1 = do
    L.logInfo () "app1 start"
    jsonInput app1' >>= jsonOutput
    L.logInfo () "app1 end"


-- for run MyApp
class (Functor f) => Run f where
    run :: f (M a) -> M a

run' :: MyApp a -> M a
run' = iterT run . runApp

instance (Run f, Run g) => Run (f :+: g) where
    run (Inl a) = run a
    run (Inr a) = run a

type instance L.OutputType () = String

instance Run (L.ILogger ()) where
    run = LM.run L.INFO

instance Run (IError String) where
    run = EM.run onError
        where onError :: String -> M a
              onError s = do
                          run' (L.logError () s :: MyApp ()) :: M ()
                          logs <- get
                          lift . Left $ (s, logs)

instance Run JsonApi where
    run (JsonInput f) = ask >>= \s ->
        case DA.decode s of
             Just i -> f i
             Nothing -> run' (throwError $ "parse error: " ++ show s :: MyApp a)

    run (JsonOutput _ c) = c



-- Arbitrary
instance Q.Arbitrary L.LogLevel where
    arbitrary = Q.elements . enumFrom $ L.DEBUG
