module Main (main) where

import Config (myConfig)
import Xmobar

main :: IO ()
main = xmobar myConfig
