import XMonad
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Layout.Fullscreen

-- Modules in ~/.xmonad/lib directory
import KeyBindings
import MouseBindings
import Layout
import WindowRules
import StartupHook
import LogHook
import EventHandling
import Defaults

main :: IO ()
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
          layoutHook = myLayoutHook, 
          handleEventHook    = myEventHook,
          logHook            = myLogHook,
          startupHook        = myStartupHook
    }
