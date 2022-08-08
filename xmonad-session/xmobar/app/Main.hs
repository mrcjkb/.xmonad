module Main (main) where

import Xmobar
import Config (myConfig)

main :: IO ()
main = xmobar myConfig
