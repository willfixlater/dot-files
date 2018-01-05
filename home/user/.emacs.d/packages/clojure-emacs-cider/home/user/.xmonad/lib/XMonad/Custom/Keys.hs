module XMonad.Custom.Keys
    ( myKeys
    , mergeMyKeys
    ) where

import XMonad.Core
    ( XConfig
        ( XConfig
        , modMask
        , terminal
        )
    , workspaces
    , spawn
    , io
    )

import XMonad.Layout
    ( ChangeLayout
        ( FirstLayout
        , NextLayout
        )
    , Resize
        ( Shrink
        , Expand
        )
    , IncMasterN(IncMasterN)
    )    

import qualified XMonad.StackSet as W
import XMonad.Operations (sendMessage, setLayout, withFocused, windows, refresh, kill)
import XMonad.Actions.PhysicalScreens (viewScreen)
import qualified Graphics.X11.Types as K
import qualified Graphics.X11.ExtraTypes.XF86 as XF
import Data.Bits ((.|.))
import Data.Map.Strict (union, fromList)
import System.Exit (exitSuccess)

mergeMyKeys keys config = union (myKeys config) (keys config)

myKeys config =
    let (XConfig {modMask = modMask}) = config
    in fromList $
        [ ((modMask .|. K.shiftMask, K.xK_x), io exitSuccess)
        , ((modMask .|. K.shiftMask, K.xK_r), spawn $
            "if type xmonad; then xmonad --recompile && xmonad --restart; " ++
            "else xmessage xmonad not in \\$PATH: \"$PATH\"; fi")

        , ((modMask .|. K.shiftMask, K.xK_d), kill)
        , ((modMask, K.xK_t), withFocused $ windows . W.sink)

        , ((modMask, K.xK_f), spawn "rofi -show run")
        , ((modMask .|. K.shiftMask, K.xK_f), spawn "rofi -show drun")
        , ((modMask, K.xK_Return), spawn $ terminal config)

        , ((modMask, K.xK_k), windows W.focusUp)
        , ((modMask .|. K.shiftMask, K.xK_k), windows W.swapUp)
        , ((modMask, K.xK_j), windows W.focusDown)
        , ((modMask .|. K.shiftMask, K.xK_j), windows W.swapDown)

        , ((modMask, K.xK_v), sendMessage NextLayout)
        , ((modMask .|. K.shiftMask, K.xK_v), sendMessage FirstLayout)

        , ((modMask, K.xK_space), windows W.focusMaster)
        , ((modMask .|. K.shiftMask, K.xK_space), windows W.swapMaster)

        , ((modMask, K.xK_h), sendMessage Shrink)
        , ((modMask, K.xK_l), sendMessage Expand)
        , ((modMask, K.xK_r), refresh)

        , ((modMask .|. K.shiftMask, K.xK_h), sendMessage (IncMasterN 1))
        , ((modMask .|. K.shiftMask, K.xK_l), sendMessage (IncMasterN (-1)))

        , ((modMask, K.xK_w), viewScreen 0)
        , ((modMask, K.xK_e), viewScreen 1)

        , ((0, XF.xF86XK_AudioMute), spawn "pamixer --toggle-mute")
        , ((0, XF.xF86XK_AudioRaiseVolume), spawn "pamixer -i 10")
        , ((0, XF.xF86XK_AudioLowerVolume), spawn "pamixer -d 10")
        ] ++
        [((modMask .|. m, k), windows $ f i)
            | (i, k) <- zip (workspaces config) [K.xK_1 .. K.xK_9]
            , (f, m) <- [(W.greedyView, 0), (W.shift, K.shiftMask)]]
