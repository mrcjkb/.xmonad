import XMonad
import XMonad.Hooks.EwmhDesktops
import XMonad.Layout.NoBorders
import XMonad.Layout.Spacing
import XMonad.Layout.Fullscreen
import XMonad.Layout.Gaps
import XMonad.Hooks.ManageDocks

-- Modules in ~/.xmonad/lib directory
import KeyBindings
import MouseBindings
import Layout
import WindowRules
import StartupHook
import LogHook
import EventHandling
import Defaults

------------------------------------------------------------------------
-- Now run xmonad with all the defaults we set up.

-- Run xmonad with the settings you specify. No need to modify this.
--
main = xmonad $ fullscreenSupport $ docks $ ewmh defaults
 where defaults = def {
          -- simple stuff
          terminal           = myTerminal,
          focusFollowsMouse  = myFocusFollowsMouse,
          clickJustFocuses   = myClickJustFocuses,
          borderWidth        = myBorderWidth,
          modMask            = myModMask,
          workspaces         = myWorkspaces,
          normalBorderColor  = myNormalBorderColor,
          focusedBorderColor = myFocusedBorderColor,

          -- key bindings
          keys               = myKeys,
          mouseBindings      = myMouseBindings,

          -- hooks, layouts
          manageHook = myManageHook, 
          layoutHook = gaps [(L,0), (R,0), (U,0), (D,0)] $ spacingRaw True (Border 0 0 0 0) True (Border 0 0 0 0) True $ smartBorders $ myLayout,
          handleEventHook    = myEventHook,
          logHook            = myLogHook,
          startupHook        = myStartupHook
    }
