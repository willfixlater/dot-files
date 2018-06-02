module XMonad.Custom.ManageHook (myManageHook) where

import XMonad.Core (XConfig(manageHook))
import XMonad.Config(def)
import XMonad.StackSet (RationalRect(..), swapDown)
import XMonad.ManageHook ((-->), (=?), composeAll, doF, className)
import XMonad.Hooks.Place (placeHook, smart)
import XMonad.Hooks.ManageHelpers (isFullscreen, doFullFloat, doRectFloat)

myManageHook = composeAll
    [ minitubeManageHook
    , doF swapDown
    , placeHook (smart (0.5, 0.5))
    , isFullscreen --> doFullFloat
    , manageHook def
    ]

minitubeManageHook =
    className =? "Minitube" --> doRectFloat minitubeRect
  where
    minitubeRect = RationalRect xPosition yPosition width height
    xPosition = 736/1366
    yPosition = 358/736
    width     = 610/1366
    height    = 210/736