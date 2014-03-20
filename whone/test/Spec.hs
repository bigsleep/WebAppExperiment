module Main where

import LoggerSpec (loggerSpec)
import JsonApiSpec (jsonApiSpec)
import JsonApiWaiTestSpec (jsonApiWaiTestSpec)
import RoutesSpec (routesSpec)

import Test.Hspec (hspec)

main :: IO ()
main = hspec $ loggerSpec >> jsonApiSpec >> jsonApiWaiTestSpec >> routesSpec
