module Main (main) where

import Xmobar
import Xmobar.Config (myConfig)

main :: IO ()
main = configFromArgs myConfig >>= xmobar
