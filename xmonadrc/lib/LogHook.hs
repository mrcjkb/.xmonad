{-# LANGUAGE FlexibleContexts #-}

module LogHook
  ( withStatusBars
  , myLogHook
  ) where

import XMonad
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP

------------------------------------------------------------------------
-- Status bars and logging

-- Perform an arbitrary action on each internal state change or X event.
-- See the 'XMonad.Hooks.StatusBar' extension for examples.

myLogHook :: X ()
myLogHook = return ()

withStatusBars :: LayoutClass l Window => XConfig l -> XConfig l
withStatusBars = withSB $ statusBarPropTo "_XMONAD_LOG_1" "xmobar -x 0" (pure pp)

-- withStatusBars = dynamicSBs barSpawner

-- barSpawner :: ScreenId -> IO StatusBarConfig
-- barSpawner = pure . xmobar
--   where
--     xmobar :: ScreenId -> StatusBarConfig
--     xmobar screenId =
--       statusBarPropTo
--         ("_XMONAD_LOG_" <> show (screenId + 1))
--         ("xmobar-app -x " <> show screenId)
--         $ pure pp

pp :: PP
pp =
  def
    { ppCurrent = xmobarColor "#E6B455" "" . wrap "[" "]"
    , ppTitle = xmobarColor "#B480D6" "" . shorten 40
    , ppVisible = wrap "(" ")"
    , ppUrgent = xmobarColor "#FF5370" "#E6B455"
    }
