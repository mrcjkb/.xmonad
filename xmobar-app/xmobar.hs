module Main (main) where

import Xmobar (configFromArgs, xmobar)

import System.Environment (getArgs)
import Xmobar.Config (mkConfig)

main :: IO ()
main = do
  print =<< getArgs
  xmobar =<< configFromArgs =<< mkConfig
