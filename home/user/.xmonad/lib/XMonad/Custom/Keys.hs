module XMonad.Custom.Keys
    ( myKeys
    , mergeMyKeys
    ) where

import XMonad ( def )

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
        [ ((modMask .|. K.shiftMask, K.xK_q), io exitSuccess)
        , ((modMask .|. K.shiftMask, K.xK_r), spawn $
            "if type xmonad; then xmonad --recompile && xmonad --restart; " ++
            "else xmessage xmonad not in \\$PATH: \"$PATH\"; fi")

        , ((modMask .|. K.shiftMask, K.xK_d), kill)
        , ((modMask, K.xK_g), withFocused $ windows . W.sink)

        , ((modMask, K.xK_space), spawn "rofi -show run")
        , ((modMask .|. K.shiftMask, K.xK_space), spawn "rofi -show drun")
        , ((modMask, K.xK_Return), spawn $ terminal config)

        , ((modMask, K.xK_f), windows W.focusUp)
        , ((modMask .|. K.shiftMask, K.xK_f), windows W.swapUp)
        , ((modMask, K.xK_b), windows W.focusDown)
        , ((modMask .|. K.shiftMask, K.xK_b), windows W.swapDown)

        , ((modMask, K.xK_s), sendMessage NextLayout)
        , ((modMask, K.xK_a), sendMessage FirstLayout)

        , ((modMask, K.xK_v), windows W.focusMaster)
        , ((modMask .|. K.shiftMask, K.xK_v), windows W.swapMaster)

        , ((modMask, K.xK_w), sendMessage Shrink)
        , ((modMask, K.xK_e), sendMessage Expand)
        , ((modMask .|. K.shiftMask, K.xK_w), sendMessage (IncMasterN 1))
        , ((modMask .|. K.shiftMask, K.xK_e), sendMessage (IncMasterN (-1)))

        , ((modMask, K.xK_x), viewScreen def 0)
        , ((modMask, K.xK_c), viewScreen def 1)
        , ((modMask, K.xK_z), viewScreen def 2)
        ] ++ [ ((modMask .|. m, k), windows $ f i)
        | (i, k) <- zip (workspaces config) [K.xK_1 .. K.xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, K.shiftMask)]
        ]
