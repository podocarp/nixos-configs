import XMonad
import XMonad.Actions.CycleWS
import XMonad.Config.Kde
import XMonad.Core
import XMonad.Hooks.DynamicBars
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Layout.Grid
import XMonad.Layout.NoBorders
import XMonad.Layout.ThreeColumns
import XMonad.Prompt
import XMonad.Prompt.ConfirmPrompt
import XMonad.Util.EZConfig(additionalKeysP)
import XMonad.Util.Scratchpad

import qualified XMonad.StackSet as W
import qualified Data.Text as T

import System.IO
import System.Exit
import Control.Monad

myXPConfig = def
  { font = "xft:DejaVu Sans Mono:size=12"
  , height = 40
  }

myKeys =
  [ ("M-S-c", confirmPrompt myXPConfig "exit" $ io exitSuccess)
  , ("M-c", spawn "xmonad --recompile; xmonad --restart") -- reload
  , ("M-S-q", kill) -- close focused window
  , ("M-S-<Return>", spawn myTerm) -- myTerm is appended by Nix
  , ("M-o", scratchpadSpawnActionTerminal myTerm)
  , ("M-d", spawn "rofi -show combi -monitor primary") -- launch dmenu
  , ("M-<Tab>", toggleWS) -- exclude those on other screens
  ]
  ++
  -- M-Shift-[1-9] moves windows to workspaces
  -- M-[1-9] greedyviews workspaces
  [("M-" ++ mask ++ show key, windows $ f i)
    | (key, i) <- zip [1..9] (workspaces def)
    , (f, mask) <- [(W.greedyView, ""), (W.shift, "S-")]]

scratchpadHook = scratchpadManageHook (W.RationalRect l t w h)
  where
    h = 0.4     -- height
    w = 0.4     -- width
    t = 1 - h   -- distance from top edge
    l = 1 - w   -- distance from left edge

myManageHook :: ManageHook
myManageHook = composeAll $
  [ className =? "TelegramDesktop" --> doFloat <+> doShift "0_5"
    , title =? "Volume Control" --> -- pavucontrol
      doRectFloat (W.RationalRect (3/8) (3/8) (1/4) (1/4))
    , (className =? "plasmashell" <&&> isInProperty "_NET_WM_STATE" "_NET_WM_STATE_SKIP_TASKBAR") -->
      doIgnore <+> hasBorder False
  ]
  ++
  [ className =? name --> doFloat | name <- [
     "About"
     , "Desktop — Plasma"
     , "Picture in picture"
     , "Picture-in-Picture"
     , "Plasma"
     , "Plasma-desktop"
     , "Preferences"
     , "dialog"
     , "krunner"
     , "menu"
     , "plasmashell"
     , "pop-up"
     , "systemsettings"
  ]]

main :: IO()
main = do
  xmonad $ kdeConfig
    { terminal = myTerm
    , modMask = mod4Mask  -- meta key
    , normalBorderColor = "#AAAAAA"
    , focusedBorderColor = "#FF0000"
    , borderWidth = 5
    , manageHook = scratchpadHook <+> myManageHook
    , layoutHook = avoidStruts $
      Tall 1 (1/100) (1/2)
      ||| ThreeColMid 1 (1/100) (25/100)
      ||| Grid
    , handleEventHook = handleEventHook def <+> fullscreenEventHook
    }
    `additionalKeysP` myKeys
