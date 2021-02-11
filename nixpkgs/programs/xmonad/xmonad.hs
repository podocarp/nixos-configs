import XMonad
import XMonad.Actions.CycleWS
import XMonad.Actions.PhysicalScreens
import XMonad.Config.Desktop
import XMonad.Core
import XMonad.Hooks.DynamicBars
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Layout.Grid
import XMonad.Layout.IndependentScreens
import XMonad.Layout.ThreeColumns
import XMonad.Prompt
import XMonad.Prompt.ConfirmPrompt
import XMonad.Util.EZConfig(additionalKeysP)
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.WorkspaceCompare

import qualified XMonad.StackSet as W
import qualified Data.ByteString.Char8 as B

import System.IO
import System.Exit
import Control.Monad

myXPConfig = def
  { font = "xft:Dina:size=12"
  , height = 40
  }

myKeys =
  [ ("M-S-c", confirmPrompt myXPConfig "exit" $ io exitSuccess)
  , ("M-c", spawn "xmonad --recompile; xmonad --restart") -- reload
  , ("M-S-q", kill) -- close focused window
  , ("M-S-<Return>", spawn myTerm) -- launch terminal
  , ("M-d", spawn "rofi -show combi") -- launch dmenu

  -- Xinerama controls, using Actions.PhysicalScreens
  , ("M-a", onPrevNeighbour def W.view)
  , ("M-s", onNextNeighbour def W.view)
  , ("M-S-a", onPrevNeighbour def W.shift)
  , ("M-S-s", onNextNeighbour def W.shift)

  , ("M-<Tab>", toggleWS) -- exclude those on other screens
  ]
  ++
  [("M-" ++ mask ++ key, f scr)
    | (key, scr) <- zip ["i", "o", "p"] [0, 1, 2]
    , (f, mask) <- [(viewScreen def, ""), (sendToScreen def, "S-")]]
  ++
  [("M-" ++ mask ++ show key, windows $ onCurrentScreen f i)
    | (key, i) <- zip [1..9] (workspaces def)
    , (f, mask) <- [(W.greedyView, ""), (W.shift, "S-")]]
  ++
  [
  -- volume controls
    ("<XF86AudioMute>", spawn muteCmd)
  , ("<XF86AudioRaiseVolume>", spawn volDownCmd)
  , ("<XF86AudioLowerVolume>", spawn volUpCmd)
  -- Brightness controls
  , ("<XF86MonBrightnessUp>", spawn "xbacklight -inc 10")
  , ("<XF86MonBrightnessDown>", spawn "xbacklight -dec 10")
  ]
  where
    muteCmd = "echo -e 'set Master toggle\nset Headphone toggle' | amixer -s"
    volDownCmd = "echo -e 'set Master 1%+\nset Headphone 1%+' | amixer -s"
    volUpCmd = "echo -e 'set Master 1%-\nset Headphone 1%-' | amixer -s"

myManageHook = composeAll
  [ className =? "TelegramDesktop" --> doFloat <+> doShift "0_5"
  , title =? "Picture-in-Picture" --> doFloat
  ]

-- This gives the hidden workspaces and the master window in those workspaces
-- as a string.
myExtras :: [X (Maybe String)]
myExtras = [withWindowSet (fmap safeUnpack . extraFormatting . getNames . W.hidden)] -- init takes out the last space
  where
    -- Gets the master window's (if any) name in the workspace
    ripName (W.Workspace i _ (Just stack)) =
      liftM2 (\x y -> B.concat [header, x, dash, y, footer]) c t
        where
          header = B.pack (i ++ ":(")
          dash = B.pack " - "
          footer = B.pack ") "
          fshorten = fmap (B.pack . shorten 13)
          t = fshorten (runQuery title (W.focus stack))
          c = fshorten (runQuery className (W.focus stack))
    ripName _ = return B.empty
    -- Given a stack of workspaces, return a list of names as per above
    getNames ws = foldl (liftM2 B.append) (return B.empty) (map ripName ws)
    extraFormatting = fmap (\s -> front `B.append` s `B.append` back)
      where
        front = B.pack "<fc=lightgray>"
        back = B.pack "</fc>"
    -- Gets the Maybe String out.
    safeUnpack s = if B.null s then Nothing else (Just . B.unpack) s

-- Coerces string to be length `n`.
-- If string is too long, cut and put ellipses, otherwise right pad with spaces.
fitStr n s
  | len < targetlen = take n (s ++ repeat ' ')
  | len == targetlen = s ++ "..."
  | otherwise = take targetlen s ++ "..."
    where
      len = length s
      targetlen = n-3

myLogHookFocused = xmobarPP
  { ppSep = " | "
  , ppCurrent = xmobarColor "green" "" . wrap "[" "]"
  , ppVisible = xmobarColor "lightgreen" "" . wrap "[" "]"
  , ppHidden = xmobarColor "gray" "" .  wrap "(" ")"
  , ppTitle = xmobarColor "cyan" "" . shorten 50      -- window title format
  , ppSort = getSortByXineramaPhysicalRule horizontalScreenOrderer
  , ppExtras = myExtras
  , ppOrder = \(ws:layout:wt:extra) -> [layout, ws, wt] ++ extra
  }

myLogHookUnfocused = myLogHookFocused
  { ppExtras = [return Nothing]
  , ppTitle = xmobarColor "lightblue" "" . shorten 200      -- window title format
  }

myDynBar :: MonadIO m => ScreenId -> m Handle
myDynBar (S n) = spawnPipe $ "xmobar -x " ++ show n

main = do
  nScreens <- countScreens
  xmonad $ desktopConfig
    { terminal = myTerm
    , modMask = mod4Mask  -- meta key
    , normalBorderColor = "#999999"
    , focusedBorderColor = "#FF0000"
    , borderWidth = 3
    , manageHook = myManageHook
    , workspaces = withScreens nScreens (workspaces def)
    , layoutHook = avoidStruts $ Tall 1 (1/100) (1/2)
      ||| ThreeColMid 1 (-1/100) (4/10)
      ||| Grid
    , startupHook = dynStatusBarStartup
        myDynBar (return ())
    , logHook = multiPP myLogHookFocused myLogHookUnfocused
    }
    `additionalKeysP` myKeys
