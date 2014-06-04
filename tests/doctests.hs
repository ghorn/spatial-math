{-# OPTIONS_GHC -Wall #-}

module Main where

import Test.DocTest

main :: IO ()
main = doctest ["src/Types.hs", "src/SpatialMath.hs"]
