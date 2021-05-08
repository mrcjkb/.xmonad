module KeyBindings where

import XMonad
import qualified WindowState as WS
import Help
import qualified XMonad.StackSet as W
import qualified Data.Map as M
import Graphics.X11.ExtraTypes.XF86 
import XMonad.Layout.Gaps
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances

import Defaults

-- Key bindings. Add, modify or remove key bindings here.
--
myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $

    -- launch a terminal
    [ ((modm .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf)
    -- launch nautilus
    , ((modm .|. shiftMask, xK_n), spawn myFileManager)
    -- launch browser
    , ((modm .|. shiftMask, xK_b), spawn myBrowser)

    -- launch rofi and dashboard
    , ((modm,               xK_o     ), spawn "rofi -show drun -theme vapor.rasi")
    , ((modm .|. shiftMask, xK_o     ), spawn "rofi -show run -theme vapor.rasi")
    --, ((modm,               xK_p     ), spawn "~/bin/centerlaunch")

    -- launch eww sidebar
    , ((modm,               xK_s     ), spawn "~/bin/sidebarlaunch")
    , ((modm .|. shiftMask, xK_s     ), spawn "exec ~/bin/ewwclose")
    
    , ((modm .|. mod1Mask, xK_x     ), spawn "xkill")
    , ((modm .|. mod1Mask, xK_k     ), spawn "inkview $HOME/git/keyboardio-atreus-firmware/atreus-layout-card.svg")
    -- Toggle floating window
    , ((modm .|. shiftMask, xK_f     ), withFocused WS.toggleFloat)
    
    -- Toggle fullscreen layout
    , ((modm,               xK_f     ), sendMessage $ Toggle FULL)    
-- Audio keys
    , ((0,                    xF86XK_AudioPlay), spawn "playerctl play-pause")
    , ((0,                    xF86XK_AudioPrev), spawn "playerctl previous")
    , ((0,                    xF86XK_AudioNext), spawn "playerctl next")
    , ((0,                    xF86XK_AudioRaiseVolume), spawn "pactl set-sink-volume @DEFAULT_SINK@ +5%")
    , ((0,                    xF86XK_AudioLowerVolume), spawn "pactl set-sink-volume @DEFAULT_SINK@ -5%")
    , ((0,                    xF86XK_AudioMute), spawn "pactl set-sink-mute @DEFAULT_SINK@ toggle")

    -- Brightness keys
    , ((0,                    xF86XK_MonBrightnessUp), spawn "brightnessctl s +10%")
    , ((0,                    xF86XK_MonBrightnessDown), spawn "brightnessctl s 10-%")
 
    -- Screenshot
    , ((0,                    xK_Print), spawn "~/bin/maimcopy")
    , ((modm,                 xK_Print), spawn "~/bin/maimsave")

    -- My Stuff
    , ((modm,               xK_b     ), spawn "exec ~/bin/bartoggle")
    , ((modm,               xK_z     ), spawn "exec ~/bin/inhibit_activate")
    , ((modm .|. shiftMask, xK_z     ), spawn "exec ~/bin/inhibit_deactivate")
    , ((modm .|. shiftMask, xK_a     ), spawn "exec ~/bin/clipboardy")

    -- close focused window
    , ((modm .|. shiftMask, xK_c     ), kill)

    -- GAPS!!!
    , ((modm .|. controlMask, xK_g), sendMessage $ ToggleGaps)               -- toggle all gaps
    

     -- Rotate through the available layout algorithms
    , ((modm,               xK_space ), sendMessage NextLayout)

    --  Reset the layouts on the current workspace to default
    , ((modm .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)

    -- Resize viewed windows to the correct size
    , ((modm,               xK_n     ), refresh)

    -- Move focus to the next window
    , ((modm,               xK_Tab   ), windows W.focusDown)

    -- Move focus to the next window
    , ((modm,               xK_j     ), windows W.focusDown)

    -- Move focus to the previous window
    , ((modm,               xK_k     ), windows W.focusUp  )

    -- Move focus to the master window
    , ((modm,               xK_m     ), windows W.focusMaster  )

    -- Swap the focused window and the master window
    , ((modm,               xK_Return), windows W.swapMaster)

    -- Swap the focused window with the next window
    , ((modm .|. shiftMask, xK_j     ), windows W.swapDown  )

    -- Swap the focused window with the previous window
    , ((modm .|. shiftMask, xK_k     ), windows W.swapUp    )

    -- Shrink the master area
    , ((modm,               xK_h     ), sendMessage Shrink)

    -- Expand the master area
    , ((modm,               xK_l     ), sendMessage Expand)

    -- Push window back into tiling
    , ((modm,               xK_t     ), withFocused $ windows . W.sink)

    -- Increment the number of windows in the master area
    , ((modm              , xK_comma ), sendMessage (IncMasterN 1))

    -- Deincrement the number of windows in the master area
    , ((modm              , xK_period), sendMessage (IncMasterN (-1)))

    -- Toggle the status bar gap
    -- Use this binding with avoidStruts from Hooks.ManageDocks.
    -- See also the statusBar function from Hooks.DynamicLog.
    --
    -- , ((modm              , xK_b     ), sendMessage ToggleStruts)

    -- Quit xmonad
    , ((modm .|. shiftMask, xK_q     ), spawn "rofi  -show fb -modi fb:~/.config/rofi/scripts/power.sh -theme vapor.rasi")

    -- Restart xmonad
    , ((modm              , xK_q     ), spawn "xmonad --recompile; xmonad --restart")

    -- Run xmessage with a summary of the default keybindings (useful for beginners)
    , ((modm .|. shiftMask, xK_h ), spawn ("echo \"" ++ help ++ "\" | xmessage -file -"))
    

    ]

    ++
 
    -- Applications --
    [  
      ((modm .|. shiftMask, xK_p     ), spawn "Polysun")
    , ((modm .|. shiftMask, xK_i     ), spawn "idea")
    , ((modm .|. mod1Mask,  xK_b     ), spawn "blueman-manager")  
    , ((modm .|. mod1Mask,  xK_v     ), spawn "pavucontrol")
    , ((modm .|. shiftMask, xK_k     ), spawn "keepassxc")
    ]


    ++

    --
    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    --
    [((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    ++

    --
    -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
    --
    [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]
