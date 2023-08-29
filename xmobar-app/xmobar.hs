module Main (main) where

import Xmobar (xmobar)

import System.Environment (getArgs)
import Xmobar.Config (mkConfig)

main :: IO ()
main = do
  print =<< getArgs
  xmobar =<< mkConfig

-- xmobar =<< configFromArgs =<< mkConfig
