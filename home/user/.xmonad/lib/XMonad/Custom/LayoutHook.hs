{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
module XMonad.Custom.LayoutHook (myLayoutHook) where

import XMonad.Layout.LayoutModifier
    ( LayoutModifier
        ( pureModifier
        , pureMess
        , modifyLayout
        , modifyDescription
        )
    , ModifiedLayout(ModifiedLayout)
    )

import Control.Arrow (second)
import Graphics.X11 (Rectangle(Rectangle))
import XMonad.Core (LayoutClass(runLayout), fromMessage)
import XMonad.Layout (Tall(Tall), Mirror(Mirror), Full(Full), (|||))
import XMonad.Layout.Spacing (ModifySpacing(..), spacingWithEdge)

myLayoutHook = tallLayout tall ||| fullLayout full ||| wideLayout wide
  where full = spacing (0, 0, 0, 0) (0, 0, 0, 0) $ Full
        tall = spacing (4, 6, 6, 4) (4, 6, 6, 4) $ Tall nMaster tallScreenDelta tallScreenRatio
        wide = spacing (4, 6, 6, 4) (4, 6, 6, 4) $ Mirror (Tall nMaster wideScreenDelta wideScreenRatio)
        nMaster = 1
        tallScreenDelta = 1/20
        wideScreenDelta = 1/20
        tallScreenRatio = 10/20
        wideScreenRatio = 17/20

-- Spacing Layout Modifier

spacing :: (Int, Int, Int, Int) -> (Int, Int, Int, Int) -> l a -> ModifiedLayout Spacing l a
spacing windows edges = ModifiedLayout (Spacing windows edges)

data Spacing a = Spacing (Int, Int, Int, Int) (Int, Int, Int, Int)
    deriving (Show, Read)

instance LayoutModifier Spacing a where
    pureModifier (Spacing (t, r, b, l) _) _ _ cws = (map (second $ shrinkRect l t (r + l) (b + t)) cws, Nothing)
    modifyLayout (Spacing _ (t, r, b, l)) wrs rect = runLayout wrs (shrinkRect l t (r + l) (b + t) rect)

shrinkRect :: Int -> Int -> Int -> Int -> Rectangle -> Rectangle
shrinkRect xd yd wd hd (Rectangle x y w h) = Rectangle x' y' w' h'
    where
    x' = x + (fromIntegral xd)
    y' = y + (fromIntegral yd)
    w' = fromIntegral $ max 1 $ (fromIntegral w) - wd
    h' = fromIntegral $ max 1 $ (fromIntegral h) - hd

-- Naming Layout Modifiers

wideLayout :: l a -> ModifiedLayout MyWideLayout l a
wideLayout = ModifiedLayout MyWideLayout

data MyWideLayout a = MyWideLayout
    deriving (Show, Read)

instance LayoutModifier MyWideLayout a where
    modifyDescription _ _ = "⬘"

tallLayout :: l a -> ModifiedLayout MyTallLayout l a
tallLayout = ModifiedLayout MyTallLayout

data MyTallLayout a = MyTallLayout
    deriving (Show, Read)

instance LayoutModifier MyTallLayout a where
    modifyDescription _ _ = "⬖"

fullLayout :: l a -> ModifiedLayout MyFullLayout l a
fullLayout = ModifiedLayout MyFullLayout

data MyFullLayout a = MyFullLayout
    deriving (Show, Read)

instance LayoutModifier MyFullLayout a where
    modifyDescription _ _ = "⯁"
