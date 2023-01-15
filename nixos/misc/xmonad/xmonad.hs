import Control.Monad (liftM2, void)
import Data.Semigroup (All)
import System.Exit (exitSuccess)
import System.IO ()
import System.Posix.Process (executeFile)
import Text.Regex.Posix ((=~))
import XMonad
  ( Default (def),
    Event,
    ManageHook,
    Query,
    ScreenId,
    Window,
    X,
    XConfig
      ( borderWidth,
        focusedBorderColor,
        handleEventHook,
        layoutHook,
        logHook,
        manageHook,
        modMask,
        normalBorderColor,
        terminal,
        workspaces
      ),
    className,
    composeAll,
    doF,
    doFloat,
    doIgnore,
    io,
    kill,
    mod4Mask,
    runQuery,
    sendMessage,
    spawn,
    stringProperty,
    title,
    windows,
    withWindowSet,
    xfork,
    xmonad,
    (-->),
    (<&&>),
    (<+>),
    (=?),
    (|||),
  )
import XMonad.Actions.CycleWS (toggleWS)
import XMonad.Actions.GridSelect
  ( GSConfig (gs_cellheight, gs_cellwidth),
    goToSelected,
  )
import XMonad.Actions.PhysicalScreens
  ( horizontalScreenOrderer,
    sendToScreen,
    viewScreen,
  )
import XMonad.Actions.UpdatePointer (updatePointer)
import XMonad.Config.Desktop (desktopConfig)
import XMonad.Config.Kde (kdeConfig)
import XMonad.Hooks.EwmhDesktops (ewmh, ewmhFullscreen)
import XMonad.Hooks.ManageDocks (avoidStruts, docks, docksEventHook)
import XMonad.Hooks.ManageHelpers (isInProperty)
import XMonad.Hooks.RefocusLast
  ( refocusLastLayoutHook,
    refocusLastWhen,
  )
import XMonad.Hooks.StatusBar
  ( StatusBarConfig,
    dynamicEasySBs,
    statusBarPropTo,
  )
import XMonad.Hooks.StatusBar.PP
  ( PP
      ( ppCurrent,
        ppHidden,
        ppOrder,
        ppSep,
        ppSort,
        ppTitle,
        ppVisible
      ),
    shorten,
    wrap,
    xmobarColor,
    xmobarPP,
  )
import XMonad.Layout.IndependentScreens
  ( VirtualWorkspace,
    countScreens,
    onCurrentScreen,
    withScreens,
  )
import XMonad.Layout.NoBorders
import XMonad.Layout.ResizableThreeColumns
  ( MirrorResize (MirrorExpand, MirrorShrink),
    ResizableThreeCol (ResizableThreeColMid),
  )
import XMonad.Layout.ResizableTile (ResizableTall (ResizableTall))
import XMonad.Prompt (XPConfig (font, height))
import XMonad.Prompt.ConfirmPrompt (confirmPrompt)
import XMonad.StackSet qualified as W
import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Util.Run (spawnPipe)
import XMonad.Util.WorkspaceCompare (getSortByTag)

myTerm = "xterm"

myXPConfig :: XPConfig
myXPConfig =
  def
    { font = "xft:DejaVu Sans Mono:size=12",
      height = 40
    }

myGsConfig :: GSConfig Window
myGsConfig =
  def
    { gs_cellheight = 20,
      gs_cellwidth = 300
    }

-- | List of workspace names, bound to keys 1 to 9 respectively.
myWorkspaces :: [VirtualWorkspace]
myWorkspaces = map show [1 .. 9]

-- This is needed to run scripts, because `spawn` uses `sh` which does not read
-- `.bashrc` and so does not know about any user defined `$PATH`.
myspawn x = void $ spawnPID x
  where
    spawnPID x = xfork $ executeFile "/usr/bin/env" False ["bash", "-c", x] Nothing

myKeys :: [(String, X ())]
myKeys =
  [ ("M-S-c", confirmPrompt myXPConfig "exit" $ io exitSuccess),
    ("M-S-q", kill), -- close focused window
    ("M-q", spawn "xmonad --restart"), -- don't recompile, nix does it for us
    ("M-S-<Return>", spawn myTerm), -- myTerm is appended by Nix
    ("M-S-h", sendMessage MirrorShrink), -- shrink slave size
    ("M-S-l", sendMessage MirrorExpand), -- expand slave size
    ("M-g", goToSelected myGsConfig),
    ("M-d", spawn "rofi -show combi"),
    ("M-f", spawn "rofi-pass"),
    ("M-p", spawn "autorandr -c"),
    ("M-<Tab>", toggleWS),
    -- screenshot and copies to clipboard
    ("<Print>", spawn "maim -s | xclip -selection clipboard -t image/png")
  ]
    -- M-Shift-[z,x,c] moves windows to screens
    -- M-[z,x,c] views screens
    ++ [ ("M-" ++ mask ++ key, f scr)
         | (key, scr) <- zip ["z", "x", "c"] [0 ..],
           (f, mask) <-
             [ (viewScreen horizontalScreenOrderer, ""),
               (sendToScreen horizontalScreenOrderer, "S-")
             ]
       ]
    ++
    -- M-Shift-[1-9] moves windows to workspaces
    -- M-[1-9] greedyviews workspaces
    [ ("M-" ++ mask ++ show key, windows $ onCurrentScreen f i)
      | (key, i) <- zip [1 .. 9] myWorkspaces,
        (f, mask) <- [(W.greedyView, ""), (W.shift, "S-")]
    ]

-- ++ [ ("<XF86AudioMute>", myspawn "changevolume toggle"),
-- ("<XF86AudioRaiseVolume>", myspawn "changevolume 5%+"),
-- ("<XF86AudioLowerVolume>", myspawn "changevolume 5%-"),
-- ("<XF86AudioMute>", spawn "echo -e 'sset Master toggle' | amixer -s")
-- , ("<XF86AudioRaiseVolume>", spawn "echo -e 'sset Master 5%+' | amixer -s")
-- , ("<XF86AudioLowerVolume>", spawn "echo -e 'sset Master 5%-' | amixer -s")
-- Brightness controls
-- ("<XF86MonBrightnessUp>", spawn "brightnessctl s 5%+"),
-- ("<XF86MonBrightnessDown>", spawn "brightnessctl s 5%-")
-- ]

-- @q =?~ x@. matches @q@ using the regex @x@, return 'True' if it matches
(=?~) :: Query String -> String -> Query Bool
q =?~ regex = fmap (matchRegex regex) q
  where
    matchRegex :: String -> String -> Bool
    matchRegex pattern string = string =~ pattern

myManageHook :: ManageHook
myManageHook =
  composeAll $
    -- ++ [ title =?~ name --> doFloat
    --      | name <-
    --          [ ".?zoom.?" -- zoom modals
    --          ]
    --   ]
    [ className =? name --> doFloat
      | name <-
          [ "About",
            "Open Folder",
            "Picture in picture",
            "Picture-in-Picture",
            "RuneLite Launcher",
            "TelegramDesktop",
            "Volume Control",
            "dialog",
            "plasmashell",
            "zoom"
          ]
    ]
      ++ [stringProperty "WM_WINDOW_ROLE" =? "pop-up" --> doFloat]
      ++ [className =? "plasmashell" <&&> isInProperty "_NET_WM_STATE" "_NET_WM_STATE_SKIP_TASKBAR" --> doIgnore]

myLayoutHook =
  smartBorders $
    avoidStruts $
      refocusLastLayoutHook $
        ResizableTall 1 (1 / 100) (1 / 2) [] ||| ResizableThreeColMid 1 (1 / 100) (30 / 100) []

myConfig nScreens =
  desktopConfig
    { terminal = myTerm,
      modMask = mod4Mask, -- meta key
      normalBorderColor = "#999999",
      focusedBorderColor = "#FF0000",
      borderWidth = 3,
      manageHook = myManageHook <+> manageHook kdeConfig,
      layoutHook = myLayoutHook,
      workspaces = withScreens nScreens myWorkspaces,
      handleEventHook = handleEventHook def,
      logHook = logHook kdeConfig
    }
    `additionalKeysP` myKeys

main :: IO ()
main = countScreens >>= \nScreens -> xmonad $ ewmhFullscreen $ ewmh $ docks (myConfig nScreens)
