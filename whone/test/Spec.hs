module Main where

import LoggerSpec (loggerSpec)
import JsonApiSpec (jsonApiSpec)

import Test.Hspec (hspec)

main :: IO ()
main = hspec $ loggerSpec >> jsonApiSpec
