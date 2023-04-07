module StartupHook (myStartupHook) where

import XMonad
import XMonad.Hooks.SetWMName
import XMonad.Util.SpawnOnce

import Control.Monad
import Data.Maybe

------------------------------------------------------------------------

-- | Startup hook

{- | Perform an arbitrary action each time xmonad starts or is restarted
 | with mod-q.  Used by, e.g., XMonad.Layout.PerWorkspace to initialize
 | per-workspace layout choices.

 | By default, do nothing.
-}
myStartupHook :: X ()
myStartupHook =
  do
    spawn "xsetroot -cursor_name left_ptr"
    -- spawn "pscircle --output=/tmp/%F_%T_$wx$h.png --background-color=202020 --dot-color-min=BC96DA --link-color-min=555555 && feh --bg-tile /tmp/%F_%T_$wx$h.png"
    spawn "autorandr -c"
    spawnOnce "greenclip daemon"
    spawnOnce "nextcloud-wrapper" -- Also spawns keepassxc
    spawnOnce "bat cache --build"
    setWMName "LG3D"
    >> addEWMHFullscreen

addNETSupported :: Atom -> X ()
addNETSupported x = withDisplay $ \dpy -> do
  r <- asks theRoot
  a_NET_SUPPORTED <- getAtom "_NET_SUPPORTED"
  a <- getAtom "ATOM"
  liftIO $ do
    sup <- join . maybeToList <$> getWindowProperty32 dpy a_NET_SUPPORTED r
    when (fromIntegral x `notElem` sup) $
      changeProperty32 dpy r a_NET_SUPPORTED a propModeAppend [fromIntegral x]

addEWMHFullscreen :: X ()
addEWMHFullscreen = do
  wms <- getAtom "_NET_WM_STATE"
  wfs <- getAtom "_NET_WM_STATE_FULLSCREEN"
  mapM_ addNETSupported [wms, wfs]
