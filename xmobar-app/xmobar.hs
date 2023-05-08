module Main (main) where

import Xmobar (configFromArgs, xmobar)
import Xmobar.Config (mkConfig)

main :: IO ()
main = do
  cfg <- mkConfig
  configFromArgs cfg >>= xmobar
