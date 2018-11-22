module XMonad.Custom.LogHook (myLogHook) where

import XMonad.ManageHook (className)
import XMonad.Hooks.FadeInactive (fadeOutLogHook, fadeIf)

myLogHook = opacityLogHook alpha
  where
    opacityLogHook = fadeOutLogHook . fadeIf isTerminalOrEditor
    alpha = 235/255

isTerminalOrEditor = do
    cn <- className
    return $ cn `elem`
        [ "Emacs"
        , "Atom"
        , "Code"
        , "URxvt"
        , "XTerm"
        ]
