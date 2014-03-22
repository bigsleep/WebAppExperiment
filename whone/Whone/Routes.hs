{-# LANGUAGE TypeOperators, FlexibleContexts, TemplateHaskell, OverloadedStrings #-}
module Whone.Routes
( Routes(..)
, RouteDefinition(..)
, RoutePattern(..)
, routes
, matchRoute
, parseRoutes
) where

import Whone.Internal

import qualified Network.HTTP.Types as HTTP (StdMethod(..))
import qualified Data.List as L (break)
import qualified Data.ByteString as B (ByteString, empty)
import qualified Data.ByteString.Char8 as B (pack, head, split)
import Control.Monad (when)
import Control.Monad.Trans.Free (FreeT(..), FreeF(..))

import qualified Language.Haskell.TH as TH (Q, Exp(..), mkName)
import qualified Language.Haskell.TH.Quote as TH (QuasiQuoter(..))


data Routes a = Routes [(RouteDefinition, a)]

instance Functor Routes where
    fmap f (Routes rs) = Routes $ fmap g rs
        where g (r, a) = (r, f a)

routes :: (Routes :<: f, Monad m) => [(RouteDefinition, App f m a)] -> App f m a
routes rs = App . FreeT . return . Free . inject $ Routes (fmap g rs)
    where g (r, a) = (r, runApp a)

data RouteDefinition = RouteDefinition {
    routeMethod :: HTTP.StdMethod,
    routePattern :: [RoutePattern]
    } deriving (Show, Eq)

data RoutePattern =
    RoutePath B.ByteString |
    RouteParameter B.ByteString
    deriving (Show, Eq)

matchRoute :: [RoutePattern] -> B.ByteString -> Maybe [(B.ByteString, B.ByteString)]
matchRoute _ "" = Nothing
matchRoute pattern url = do
    when (B.head url /= '/') Nothing
    let route = filter (/= "") . B.split '/' $ url
    when (length pattern /= length route) Nothing
    sequence . filter (/= emp) . fmap match $ zip pattern route
    where match (RoutePath a, b) = if a == b then emp else Nothing
          match (RouteParameter a, b) = Just (a, b)
          emp = Just (B.empty, B.empty)

parseRoutes :: TH.QuasiQuoter
parseRoutes = TH.QuasiQuoter
    { TH.quoteExp = parseRoutesFromString
    , TH.quotePat = undefined
    , TH.quoteType = undefined
    , TH.quoteDec = undefined
    }

parseRoutesFromString :: String -> TH.Q TH.Exp
parseRoutesFromString s = do
    rs <- mapM parseEntry . filter (/= "") . lines $ s
    f <- [|routes|]
    return $ TH.AppE f (TH.ListE rs)

parseEntry :: String -> TH.Q TH.Exp
parseEntry = f . words
    where f (m : r : a : []) = do
                               xs <- [|RouteDefinition (read m) (parseRoute r)|]
                               return $ TH.TupE [xs, TH.VarE (TH.mkName a)]
          f _ = error "invalid entry pattern"

parseRoute :: String -> [RoutePattern]
parseRoute ('/' : s) = filter (/= RoutePath "") . split $ s
    where split a = case L.break (== '/') a of
                         (h, '/' : t) -> routePatternFromString h : split t
                         (h, _) -> [routePatternFromString h]
parseRoute _ = error "invalid route pattern"

routePatternFromString :: String -> RoutePattern
routePatternFromString ":" = error "invalid route parameter pattern"
routePatternFromString (':' : s) = RouteParameter $ B.pack s
routePatternFromString s = RoutePath $ B.pack s
