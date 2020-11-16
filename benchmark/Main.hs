module Main (main) where

import Gauge.Main
import Protolude

main :: IO ()
main = defaultMain [bench "const" (whnf const ())]
