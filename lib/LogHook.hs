{-# LANGUAGE FlexibleContexts #-}

module LogHook
  ( withStatusBar
  , myLogHook
  ) where

import XMonad
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP

------------------------------------------------------------------------
-- Status bars and logging

-- Perform an arbitrary action on each internal state change or X event.
-- See the 'XMonad.Hooks.StatusBar' extension for examples.
--
-- TODO: Add xmobarrc here

myLogHook :: X ()
myLogHook = return ()

withStatusBar :: LayoutClass l Window => XConfig l -> XConfig l
withStatusBar = withSB $ statusBarProp "xmobar" (pure xmobarPP)

