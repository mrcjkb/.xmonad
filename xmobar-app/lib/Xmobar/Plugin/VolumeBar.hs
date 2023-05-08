{-# LANGUAGE ScopedTypeVariables #-}

module Xmobar.Plugin.VolumeBar (VolumeBar (..)) where

import System.Process (readProcess)
import Xmobar

data VolumeBar = VolumeBar
  deriving (Read, Show)

instance Exec VolumeBar where
  alias VolumeBar = "volbar"
  rate VolumeBar = 5
  run VolumeBar = do
    volume <- parse <$> readProcess "pamixer" ["--get-volume"] []
    pure $ slider volume 10 setVolumeCmd "\62532" "·"
    where
      parse v = read (takeWhile (/= '%') v) :: Int
      setVolumeCmd v = "pamixer --set-volume " ++ show (10 * v)

slider :: Int -> Int -> (Int -> String) -> String -> String -> String
slider value barWidth mkSetVolumeCmd filledSymbol emptySymbol = wrap "\60196 " " \60277" bar
  where
    percentage :: Double = fromIntegral value / 100

    filledSize = ceiling (fromIntegral barWidth * percentage)

    mkSymbol :: Int -> String
    mkSymbol i =
      if i < filledSize
        then filledSymbol
        else emptySymbol

    bar = concat [action (mkSetVolumeCmd i) (mkSymbol i) | i <- [0 .. barWidth - 1]]

    wrap :: String -> String -> String -> String
    wrap a1 a2 b = a1 <> b <> a2

    action :: String -> String -> String
    action command = wrap ("<action=" <> command <> ">") "</action>"
