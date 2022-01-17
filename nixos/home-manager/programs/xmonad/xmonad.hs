import XMonad
import XMonad.Actions.CycleWS
import XMonad.Actions.GridSelect
import XMonad.Config.Desktop
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
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeysP)
import XMonad.Util.Scratchpad
import XMonad.Util.WorkspaceCompare

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
  , ("M-S-q", kill) -- close focused window
  , ("M-S-<Return>", spawn myTerm) -- myTerm is appended by Nix
  , ("M-o", scratchpadSpawnActionTerminal myTerm)
  , ("M-g", goToSelected def)
  , ("M-d", spawn "rofi -show combi") -- launch dmenu
  , ("M-f", spawn "rofi-pass") -- launch dmenu
  , ("M-<Tab>", toggleWS) -- exclude those on other screens
  ]
  ++
  -- M-Shift-[1-9] moves windows to workspaces
  -- M-[1-9] greedyviews workspaces
  [("M-" ++ mask ++ show key, windows $ f i)
    | (key, i) <- zip [1..9] (workspaces def)
    , (f, mask) <- [(W.greedyView, ""), (W.shift, "S-")] ]
  ++
  [ ("<XF86AudioMute>", spawn muteCmd)
  , ("<XF86AudioRaiseVolume>", spawn volDownCmd)
  , ("<XF86AudioLowerVolume>", spawn volUpCmd)
  -- Brightness controls
  , ("<XF86MonBrightnessUp>", spawn "brightnessctl s 1%+")
  , ("<XF86MonBrightnessDown>", spawn "brightnessctl s 1%-")
  ]
  where
    -- This might be needed on ALSA.
    -- "echo -e 'sset Master toggle\nset Headphone toggle'
    muteCmd = "echo -e 'sset Master toggle' | amixer -s"
    volDownCmd = "echo -e 'sset Master 1%+' | amixer -s"
    volUpCmd = "echo -e 'sset Master 1%-' | amixer -s"

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

-- This gives the hidden workspaces and the master window in those workspaces
-- as a string.
myExtras :: [X (Maybe String)]
myExtras = [withWindowSet (fmap safeUnpack . extraFormatting . getNames . W.hidden)] -- init takes out the last space
  where
    -- Gets the master window's (if any) name in the workspace
    ripName (W.Workspace i _ (Just stack)) =
      liftM2 (\x y -> T.concat [header, x, dash, y, footer]) c t
        where
          header = T.pack (i ++ ":(")
          dash = T.pack " - "
          footer = T.pack ") "
          fshorten = fmap (T.pack . shorten 13)
          t = fshorten (runQuery title (W.focus stack))
          c = fshorten (runQuery className (W.focus stack))
    ripName _ = return T.empty
    -- Given a stack of workspaces, return a list of names as per above
    getNames ws = foldl (liftM2 T.append) (return T.empty) (map ripName ws)
    extraFormatting = fmap (\s -> front `T.append` s `T.append` back)
      where
        front = T.pack "<fc=lightgray>"
        back = T.pack "</fc>"
    -- Gets the Maybe String out.
    safeUnpack s = if T.null s then Nothing else (Just . T.unpack) s

-- Coerces string to be length `n`.
-- If string is too long, cut and put ellipses, otherwise right pad with spaces.
fitStr n s
  | len < targetlen = take n (s ++ repeat ' ')
  | len == targetlen = s ++ "..."
  | otherwise = take targetlen s ++ "..."
    where
      len = length s
      targetlen = n-3

myPP = xmobarPP {
    ppSep = " | "
  , ppCurrent = xmobarColor "green" "" . wrap "[" "]"
  , ppVisible = xmobarColor "lightgreen" "" . wrap "[" "]"
  , ppHidden = xmobarColor "gray" "" .  wrap "(" ")"
  , ppTitle = xmobarColor "cyan" "" . shorten 50      -- window title format
  , ppSort = getSortByIndex
  , ppOrder = \(ws:layout:wt:extra) -> [layout, ws, wt] ++ extra
  }

myDynBar :: MonadIO m => ScreenId -> m Handle
myDynBar (S n) = spawnPipe $ "xmobar -x " ++ show n

main :: IO()
main = do
  bar <- spawnPipe "xmobar"
  xmonad $ docks $ ewmh desktopConfig
    { terminal = myTerm
    , modMask = mod4Mask  -- meta key
    , normalBorderColor = "#AAAAAA"
    , focusedBorderColor = "#FF0000"
    , borderWidth = 3
    , manageHook = scratchpadHook <+> myManageHook
    , layoutHook = avoidStruts $
      Tall 1 (1/100) (1/2)
      ||| ThreeColMid 1 (1/100) (25/100)
      ||| Grid
    , handleEventHook = handleEventHook def <+> fullscreenEventHook
    , logHook = dynamicLogWithPP $ myPP { ppOutput = hPutStrLn bar }
    }
    `additionalKeysP` myKeys
