{-# LANGUAGE FlexibleContexts #-}
module XMonad.Custom.StatusBar (myStatusBar) where

import XMonad.Hooks.DynamicLog
    ( statusBar
    , def
    , ppOrder
    , ppSep
    , ppLayout
    , ppCurrent
    , ppVisible
    , ppHidden
    , ppHiddenNoWindows
    , ppUrgent
    , wrap
    , xmobarColor
    )

-- TODO: Fix 6px top border between status bar and client windows
myStatusBar config = statusBar polybar ppOptions toggleVisibility config
  where polybar = "~/.bin/run-polybar.sh"
        toggleVisibility _ = (0,0)
        ppOptions = def
            { ppOrder = \(ws:l:t:_) -> [l, ws]
            , ppSep = " • "
            , ppLayout = id
            , ppHidden = id
            , ppHiddenNoWindows = id
            , ppCurrent = id . xmobarColor "#000000" ""
            , ppVisible = id . xmobarColor "#000000" ""
            , ppUrgent  = id . xmobarColor "#cc6666" ""
            }

useFont n = wrap ("<fn=" ++ show n ++ ">") "</fn>"

dotBelow ws
    | ws == "1" = "1̣"
    | ws == "2" = "2̣"
    | ws == "3" = "3̣"
    | ws == "4" = "4̣"
    | ws == "5" = "5̣"
    | ws == "6" = "6̣"
    | ws == "7" = "7̣"
    | ws == "8" = "8̣"
    | ws == "9" = "9̣"
    | otherwise = ws
