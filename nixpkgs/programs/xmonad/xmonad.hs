import XMonad
import XMonad.Actions.PhysicalScreens
import XMonad.Config.Desktop
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.EZConfig(additionalKeysP)
import XMonad.Util.Run(spawnPipe)
import XMonad.Prompt.ConfirmPrompt

import qualified XMonad.StackSet as W

import System.IO
import System.Exit

myKeys =
  [ ("M-S-c", confirmPrompt def "exit" $ io (exitWith ExitSuccess))
  , ("M-c", spawn "xmonad --recompile; xmonad --restart") -- reload
  , ("M-S-q", kill) -- close focused window
  , ("M-S-<Return>", spawn myTerm) -- launch terminal
  , ("M-d", spawn "rofi -show run") -- launch dmenu

  -- Xinerama controls, using Actions.PhysicalScreens
  , ("M-a", onPrevNeighbour def W.view)
  , ("M-s.", onNextNeighbour def W.view)
  , ("M-S-a", onPrevNeighbour def W.shift)
  , ("M-S-s", onNextNeighbour def W.shift)
  ]
  ++
  [("M-" ++ mask ++ key, f scr)
    | (key, scr) <- zip ["w", "e", "r"] [0, 1, 2]
    , (f, mask) <- [(viewScreen def, ""), (sendToScreen def, "S")]]
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
  ]

main = do
  xmproc <- spawnPipe "xmobar"

  xmonad $ desktopConfig
      { terminal = myTerm
      , modMask = mod4Mask  -- meta key
      , normalBorderColor = "#555555"
      , focusedBorderColor = "#FF0000"
      , manageHook = myManageHook <+> manageHook defaultConfig
      , logHook = dynamicLogWithPP xmobarPP
        {
          ppOutput = hPutStrLn xmproc
        , ppTitle = xmobarColor "green" "" . shorten 50
        }
      }
      `additionalKeysP` myKeys
