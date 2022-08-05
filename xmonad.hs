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
 where 
  defaults = def 
    { terminal           = myTerminal
    , focusFollowsMouse  = myFocusFollowsMouse
    , clickJustFocuses   = myClickJustFocuses
    , borderWidth        = myBorderWidth
    , modMask            = myModMask
    , workspaces         = myWorkspaces
    , normalBorderColor  = myNormalBorderColor
    , focusedBorderColor = myFocusedBorderColor
    , keys               = myKeys
    , mouseBindings      = myMouseBindings
    , manageHook         = myManageHook
    , layoutHook         = myLayoutHook 
    , handleEventHook    = myEventHook
    , logHook            = myLogHook
    , startupHook        = myStartupHook
    }
