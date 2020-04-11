import XMonad
  ( xmonad
  , def
  , modMask
  , mod3Mask
  , borderWidth
  , clickJustFocuses
  , focusFollowsMouse
  , workspaces
  , keys
  , layoutHook
  )

import XMonad.Custom.Keys (myKeys)
import XMonad.Custom.LayoutHook (myLayoutHook)

main = xmonad def
  { modMask = mod3Mask
  , borderWidth = 0
  , clickJustFocuses = False
  , focusFollowsMouse = False
  , workspaces = map show [1..9]
  , keys = myKeys
  , layoutHook = myLayoutHook
  }
