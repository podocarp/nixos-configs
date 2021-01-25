import XMonad
import XMonad.Actions.CycleWS
import XMonad.Actions.PhysicalScreens
import XMonad.Actions.WorkspaceNames
import XMonad.Config.Desktop
import XMonad.Core
import XMonad.Hooks.DynamicBars
import XMonad.Hooks.DynamicLog
import XMonad.Layout.IndependentScreens
import XMonad.Prompt.ConfirmPrompt
import XMonad.Util.EZConfig(additionalKeysP)
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.WorkspaceCompare

import qualified XMonad.StackSet as W
import qualified Data.ByteString.Char8 as B

import System.IO
import System.Exit
import Control.Monad

myKeys =
  [ ("M-S-c", confirmPrompt def "exit" $ io exitSuccess)
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
  , ("M-n", renameWorkspace def)
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
  -- TP X200 doesn't have the keys above
  , ("<XF86AudioStop>", spawn muteCmd)
  , ("<XF86AudioNext>", spawn volDownCmd)
  , ("<XF86AudioPrev>", spawn volUpCmd)
  -- Brightness controls
  , ("<XF86MonBrightnessUp>", spawn "xbacklight -inc 10")
  , ("<XF86MonBrightnessDown>", spawn "xbacklight -dec 10")
  ]
  where
    muteCmd = "echo -e 'set Master toggle\nset Headphone toggle' | amixer -s"
    volDownCmd = "echo -e 'set Master 2dB+\nset Headphone 2dB+' | amixer -s"
    volUpCmd = "echo -e 'set Master 2dB-\nset Headphone 2dB-' | amixer -s"

myManageHook = composeAll
  [ className =? "Firefox" --> doShift "9"
  , className =? "Chromium" --> doShift "8"
  ]

-- This gives the hidden workspaces and the master window in those workspaces
-- as a string.
myExtras :: [X (Maybe String)]
myExtras = [withWindowSet (fmap safeInit . gn . W.hidden)] -- init takes out the last space
  where
    -- Gets the master window's (if any) name in the workspace
    ripName (W.Workspace i _ (Just stack)) =
      liftM2 (\x y -> header `B.append` x `B.append` dash `B.append` y `B.append` footer) c t
        where
          header = B.pack (i ++ ":(")
          dash = B.pack " - "
          footer = B.pack ") "
          fshort = fmap (B.pack . shorten 10)
          t = fshort (runQuery title (W.focus stack))
          c = fshort (runQuery className (W.focus stack))

    ripName _ = return B.empty
    -- Given a stack of workspaces, return a list of names as per above
    gn ws = foldl (liftM2 B.append) (return B.empty) (map ripName ws)
    -- Gets all but the last element of the bytestring
    safeInit:: B.ByteString -> Maybe String
    safeInit s = if B.null s then Nothing else (Just . B.unpack . B.init) s

myLogHookFocused = xmobarPP
  { ppSep = " | "
  , ppCurrent = xmobarColor "lightgreen" "" . wrap "[" "]"
  , ppVisible = xmobarColor "green" "" . wrap "[" "]"
  , ppHidden = const ""
  , ppTitle = xmobarColor "cyan" "" . shorten 100      -- window title format
  , ppSort = getSortByXineramaPhysicalRule horizontalScreenOrderer
  , ppExtras = myExtras
  , ppOrder = \(ws:layout:wt:extra) -> [layout, ws] ++ extra ++ [wt]
  }

myLogHookUnfocused = myLogHookFocused
  { ppExtras = [return Nothing]
  , ppTitle = xmobarColor "lightblue" "" . shorten 100      -- window title format
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
    , borderWidth = 5
    , workspaces = withScreens nScreens (workspaces def)
    , startupHook = dynStatusBarStartup
        myDynBar (return ())
    , logHook = multiPP myLogHookFocused myLogHookUnfocused
    }
    `additionalKeysP` myKeys
