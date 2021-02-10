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

-- The preferred terminal program, which is used in a binding below and by
myTerminal :: String
myTerminal = "alacritty"

-- Whether focus follows the mouse pointer.
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

-- Whether clicking on a window to focus also passes the click to the window
myClickJustFocuses :: Bool
myClickJustFocuses = False

-- Width of the window border in pixels.
--
myBorderWidth = 2

-- modMask lets you specify which modkey you want to use. The default
-- is mod1Mask ("left alt").  You may also consider using mod3Mask
-- ("right alt"), which does not conflict with emacs keybindings. The
-- "windows key" is usually mod4Mask.
--
myModMask = mod4Mask

-- The default number of workspaces (virtual screens) and their names.
-- By default we use numeric strings, but any string may be used as a
-- workspace name. The number of workspaces is determined by the length
-- of this list.
--
-- A tagging example:
--
-- > workspaces = ["web", "irc", "code" ] ++ map show [4..9]
--
myWorkspaces :: [String]
myWorkspaces = map show [1..9]

-- Border colors for unfocused and focused windows, respectively.
--
myNormalBorderColor :: String
myNormalBorderColor  = "#3b4252"
myFocusedBorderColor :: String
myFocusedBorderColor = "#bc96da"


------------------------------------------------------------------------
-- Event handling

-- * EwmhDesktops users should change this to ewmhDesktopsEventHook
--
-- Defines a custom handler function for X Events. The function should
-- return (All True) if the default handler is to be run afterwards. To
-- combine event hooks use mappend or mconcat from Data.Monoid.
--
myEventHook = mempty


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
