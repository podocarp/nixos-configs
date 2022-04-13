import XMonad
import XMonad.Actions.CycleWS
import XMonad.Actions.GridSelect
import XMonad.Config.Desktop
import XMonad.Core
import XMonad.Hooks.StatusBar
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

scratchpadHook = scratchpadManageHook (W.RationalRect l t w h)
  where
    h = 0.4     -- height
    w = 0.4     -- width
    t = 1 - h   -- distance from top edge
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
     , "dolphin"
     , "mpv"
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
  xmonad $ docks $ ewmhFullscreen desktopConfig
    { terminal = myTerm
    , modMask = mod4Mask  -- meta key
    , normalBorderColor = "#AAAAAA"
    , focusedBorderColor = "#FF0000"
    , borderWidth = myBorderWidth
    , manageHook = scratchpadHook <+> myManageHook
    , layoutHook = avoidStruts $
      Tall 1 (1/100) (1/2)
      ||| ThreeColMid 1 (1/100) (25/100)
      ||| Grid
    , handleEventHook = handleEventHook def
    , logHook = dynamicLogWithPP $ myPP { ppOutput = hPutStrLn bar }
    }
    `additionalKeysP` myKeys
