module Main (main) where

import Xmobar (configFromArgs, xmobar)
import Xmobar.Config (myConfig)

main :: IO ()
main = configFromArgs myConfig >>= xmobar
