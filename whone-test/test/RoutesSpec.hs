{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}
module RoutesSpec
( routesSpec
) where

import Whone.Internal (App(..))
import Whone.Routes (Routes(..), parseRoutes, routeMethod, routePattern, matchRoute)

import Data.Maybe (isJust, fromJust)
import qualified Data.List as L (find, lookup)
import qualified Network.HTTP.Types as HTTP (StdMethod(..))
import qualified Data.ByteString as B (ByteString, concat)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Free (iterT)
import Control.Monad.Reader (ReaderT(..), runReaderT, ask)

import Test.Hspec (Spec, describe, it, shouldBe)

routesSpec :: Spec
routesSpec = describe "Routes" $
    it "dispatch correctly" $ do
        test HTTP.GET "/" `shouldBe` Just "root"
        test HTTP.POST "/" `shouldBe` Nothing
        test HTTP.GET "/////////////" `shouldBe` Just "root"
        test HTTP.GET "/news" `shouldBe` Just "news"
        test HTTP.GET "/blog/2014/03/21" `shouldBe` Just "blog-2014-03-21"
        test HTTP.GET "/blog////2014///03//21/////" `shouldBe` Just "blog-2014-03-21"
        test HTTP.GET "/blog/2013/03/21" `shouldBe` Just "blog-2013-03-21"
        test HTTP.GET "/blog/2014/03/21/00/00" `shouldBe` Nothing
        test HTTP.POST "/user/register" `shouldBe` Just "register"
    where test = curry . runReaderT . run' $ myapp

myapp :: MyApp B.ByteString
myapp = [parseRoutes|
GET     /                           root
GET     /news                       news
GET     /blog/:year/:month/:day     blog
POST    /user/register              register
|]

type MyApp = App Routes M
type M = ReaderT (HTTP.StdMethod, B.ByteString) Maybe

root :: MyApp B.ByteString
root = return "root"

news :: MyApp B.ByteString
news = return "news"

blog :: [(B.ByteString, B.ByteString)] -> MyApp B.ByteString
blog params = do
    let year = getParam "year"
    let month = getParam "month"
    let day = getParam "day"
    return . B.concat $ ["blog-", year, "-", month, "-", day]
    where getParam n = fromJust (L.lookup n params)

register :: MyApp B.ByteString
register = return "register"


--
run' :: MyApp a -> M a
run' = iterT run . runApp

class Run f where
    run :: f (M a) -> M a

instance Run Routes where
    run (Routes rs) = do
        (m, url) <- ask
        case L.find isJust (g m url rs) of
             Just (Just a) -> a
             _ -> lift Nothing
        where g m url = fmap (h url) . filter ((== m) . routeMethod . fst)
              h url (r, a) = matchRoute (routePattern r) url >>= Just . a
