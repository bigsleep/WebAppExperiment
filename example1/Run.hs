{-# LANGUAGE TypeOperators, FlexibleContexts, FlexibleInstances, OverloadedStrings, TypeFamilies #-}
module Run
( runWebApp
, F
, M
) where

import Whone.Internal (App(..), (:+:)(..))
import Whone.JsonApi (JsonApi(..), HttpError(..))
import Whone.Routes (Routes(..), matchRoute, routeMethod, routePattern)
import Whone.Error (IError(..))
import Whone.Logger (ILogger(..), OutputType, LogLevel(..))

import Data.Maybe (isJust)
import qualified Data.List as L (find)
import qualified Data.Aeson as DA (encode, decode)
import qualified Data.ByteString as B (ByteString)
import qualified Data.ByteString.Lazy as LBS (ByteString, fromChunks)
import qualified Data.Conduit as C (Sink, ($$))
import qualified Data.Conduit.List as CL (consume)

import Control.Monad.Reader (ask)
import Control.Monad.State (put)
import Control.Monad.Writer (tell)
import Control.Monad.RWS (RWST, runRWST)
import Control.Monad.Trans.Free (iterT)
import Control.Monad.Trans (lift)

import qualified Network.Wai as W (Request, Response, Application, requestMethod, requestBody, rawPathInfo, responseLBS)
import qualified Network.HTTP.Types as HTTP (status200, status404, parseMethod)
import qualified Network.Wai.Handler.Warp as Warp (run)

type F = Routes :+: JsonApi :+: IError HttpError :+: ILogger ()
type M = RWST (W.Request, LBS.ByteString) [(LogLevel, String)] W.Response (Either HttpError)
type instance OutputType () = String

run' :: App F M a -> M a
run' = iterT run . runApp

class Run f where
    run :: f (M a) -> M a

instance (Run f, Run g) => Run (f :+: g) where
    run (Inl a) = run a
    run (Inr a) = run a

instance Run JsonApi where
    run (JsonInput f) = do
        (_, body) <- ask
        case DA.decode body of
             Just i -> f i
             _ -> lift . Left $ HttpError HTTP.status404 [] "error"
    run (JsonOutput o a) = (put . W.responseLBS HTTP.status200 header . DA.encode $ o) >> a
        where header = []

instance Run Routes where
    run (Routes rs) = do
        (request, _) <- ask
        let m = W.requestMethod request
        let url = W.rawPathInfo request
        case L.find isJust (g m url rs) of
             Just (Just a) -> a
             _ -> lift . Left $ HttpError HTTP.status404 [] "error"
        where g m url = fmap (h url) . filter ((== HTTP.parseMethod m) . Right . routeMethod . fst)
              h url (r, a) = matchRoute (routePattern r) url >>= Just . a

instance Run (IError HttpError) where
    run (ThrowError e) = lift (Left e)

instance Run (ILogger ()) where
    run (GetCurrentLogLevel _ f) = f DEBUG
    run (Log () lv l c) = tell [(lv, l)] >> c

toApplication :: App F M () -> W.Application
toApplication app request = W.requestBody request C.$$ sink
    where sink :: C.Sink B.ByteString IO W.Response
          sink = do
                 i <- CL.consume
                 let body = LBS.fromChunks i
                 case runRWST (run' app) (request, body) defaultResponse of
                      Right (_, response, _) -> return response
                      Left (HttpError s h b) -> return $ W.responseLBS s h b
          defaultResponse = W.responseLBS HTTP.status404 [] "error"

runWebApp :: App F M () -> IO ()
runWebApp app = Warp.run 3000 (toApplication app)
