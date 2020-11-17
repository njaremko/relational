module Main (main) where

import Gauge.Main
import Relude

main :: IO ()
main = defaultMain [bench "const" (whnf const ())]
