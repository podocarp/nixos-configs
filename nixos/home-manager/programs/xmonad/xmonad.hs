{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
import XMonad
import XMonad.Actions.CycleWS (toggleWS)
import XMonad.Actions.GridSelect (goToSelected)
import XMonad.Config.Desktop (desktopConfig)
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops (ewmhFullscreen)
import XMonad.Hooks.ManageDocks
import XMonad.Layout.ResizableTile
import XMonad.Layout.ResizableThreeColumns
import XMonad.Prompt
import XMonad.Prompt.ConfirmPrompt
import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Util.Run (spawnPipe)
import XMonad.Util.Scratchpad (scratchpadManageHook, scratchpadSpawnActionTerminal)
import XMonad.Util.WorkspaceCompare (getSortByIndex)

import qualified XMonad.StackSet as W
import qualified Data.Text as T

import System.IO
import System.Exit
import Control.Monad

myXPConfig :: XPConfig
myXPConfig = def
  { font = "xft:DejaVu Sans Mono:size=12"
  , height = 40
  }

myKeys :: [(String, X())]
myKeys =
  [ ("M-S-c", confirmPrompt myXPConfig "exit" $ io exitSuccess)
  , ("M-S-q", kill) -- close focused window
  , ("M-S-<Return>", spawn myTerm) -- myTerm is appended by Nix
  , ("M-S-h", sendMessage MirrorShrink) -- shrink slave size
  , ("M-S-l", sendMessage MirrorExpand) -- expand slave size
  , ("M-o", scratchpadSpawnActionTerminal myTerm)
  , ("M-g", goToSelected def)
  , ("M-d", spawn "rofi -show combi")
  , ("M-f", spawn "rofi-pass")
  , ("M-e", spawn "dolphin")
  , ("M-<Tab>", toggleWS) -- exclude those on other screens
  -- screenshot and copies to clipboard
  , ("<Print>", spawn "scrot -s -e 'xclip -selection clipboard -t image/png -i $f'")
  ]
  ++
  -- M-Shift-[1-9] moves windows to workspaces
  -- M-[1-9] greedyviews workspaces
  [("M-" ++ mask ++ show key, windows $ f i)
    | (key, i) <- zip [1..9] (workspaces def)
    , (f, mask) <- [(W.greedyView, ""), (W.shift, "S-")] ]
  ++
  [ ("<XF86AudioMute>", spawn "echo -e 'sset Master toggle' | amixer -s")
  , ("<XF86AudioRaiseVolume>", spawn "echo -e 'sset Master 1%+' | amixer -s")
  , ("<XF86AudioLowerVolume>", spawn "echo -e 'sset Master 1%-' | amixer -s")
  -- Brightness controls
  , ("<XF86MonBrightnessUp>", spawn "brightnessctl s 1%+")
  , ("<XF86MonBrightnessDown>", spawn "brightnessctl s 1%-")
  ]

scratchpadHook :: ManageHook
scratchpadHook = scratchpadManageHook (W.RationalRect l t w h)
  where
    h = 0.5     -- height
    w = 0.3     -- width
    t = 0.9 - h   -- distance from top edge
    l = 1 - w   -- distance from left edge

myManageHook :: ManageHook
myManageHook = composeAll $
  [ title =? name --> doFloat | name <- [
    "Volume Control"
    , "Open Folder"
  ]]
  ++
  [ className =? name --> doFloat | name <- [
     "About"
     , "Image Lounge" -- nomacs
     , "Picture in picture"
     , "Picture-in-Picture"
     , "TelegramDesktop"
     , "dialog"
     , "mpv"
  ]]

myPP :: PP
myPP = xmobarPP {
    ppSep = " | "
  , ppCurrent = xmobarColor "green" "" . wrap "[" "]"
  , ppVisible = xmobarColor "lightgreen" "" . wrap "[" "]"
  , ppHidden = xmobarColor "gray" "" .  wrap "(" ")"
  , ppTitle = xmobarColor "cyan" "" -- window title format
  , ppSort = getSortByIndex
  , ppOrder = \(ws:layout:wt:extra) -> [layout, ws, wt] ++ extra
  }

myHandleEventHook = handleEventHook def

myLayoutHook = avoidStruts $
  ResizableTall 1 (1/100) (1/2) []
  ||| ResizableThreeColMid 1 (1/100) (30/100) []

main :: IO()
main = do
  bar <- spawnPipe "xmobar"
  xmonad $ docks $ ewmhFullscreen desktopConfig
    { terminal = myTerm
    , modMask = mod4Mask  -- meta key
    , normalBorderColor = "#999999"
    , focusedBorderColor = "#FF0000"
    , borderWidth = 5
    , manageHook = scratchpadHook <+> myManageHook
    , layoutHook = myLayoutHook
    , handleEventHook = myHandleEventHook
    , logHook = dynamicLogWithPP $ myPP { ppOutput = hPutStrLn bar }
    }
    `additionalKeysP` myKeys
