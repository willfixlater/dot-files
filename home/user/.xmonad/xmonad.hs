import XMonad

import XMonad.Util.SpawnOnce (spawnOnce)
import XMonad.Custom.Keys (myKeys)
import XMonad.Custom.LogHook (myLogHook)
import XMonad.Custom.LayoutHook(myLayoutHook)
import XMonad.Custom.ManageHook(myManageHook)
import XMonad.Custom.StatusBar(myStatusBar)

main = xmonad =<< myStatusBar myConfig
  where
    myConfig = def
        { modMask = mod4Mask
        , terminal = "urxvt"
        , borderWidth = 0
        , workspaces = map show [1..9]
        , clickJustFocuses = False
        , focusFollowsMouse = False
        , keys = myKeys
        , logHook = myLogHook
        , layoutHook = myLayoutHook
        , manageHook = myManageHook
        , startupHook = spawnOnce "~/.fehbg"
        }
