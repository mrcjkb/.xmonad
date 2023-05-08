{-# LANGUAGE ScopedTypeVariables #-}

module Xmobar.Plugin.VolumeBar (VolumeBar (..)) where

import System.Process (readProcess)
import Xmobar

data VolumeBar = VolumeBar
  deriving (Read, Show)

instance Exec VolumeBar where
  rate VolumeBar = 5
  run VolumeBar = do
    vol <- readProcess "pamixer" ["--get-volume"] []
    return (slider (parse vol) 100 10 setVolume "#" "·")
    where
      parse v = read (takeWhile (/= '%') v) :: Int
      setVolume v = "pamixer --set-volume " ++ show (10 * v)

slider :: Int -> Int -> Int -> (Int -> String) -> String -> String -> String
slider value total width_ fun filledSymbol emptySymbol = wrap "<" ">" bar
  where
    percentage :: Double = fromIntegral value / fromIntegral total
    filledSize = ceiling (fromIntegral width_ * percentage)
    -- bar = replicate filledSize 'X' ++ replicate (width - filledSize) '-'
    symbol i = if i < filledSize then filledSymbol else emptySymbol
    bar = concat [action (fun i) (symbol i) | i <- [0 .. width_ - 1]]

    wrap :: String -> String -> String -> String
    wrap a1 a2 b = a1 <> b <> a2

    action :: String -> String -> String
    action command = wrap ("<action=" ++ command ++ ">") "</action>"
