import Control.Monad (liftM2, void)
import Data.Semigroup (All)
import System.Exit (exitSuccess)
import System.IO ()
import System.Posix.Process (executeFile)
import Text.Regex.Posix ((=~))
import XMonad
  ( Default (def),
    ManageHook,
    Query,
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
    appName,
    className,
    composeAll,
    doFloat,
    doIgnore,
    io,
    kill,
    mod4Mask,
    sendMessage,
    spawn,
    stringProperty,
    windows,
    xfork,
    xmonad,
    (-->),
    (<&&>),
    (<+>),
    (=?),
    (|||),
  )
import XMonad.Actions.CycleWS (toggleWS, toggleWS')
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
import XMonad.Config.Prime (ScreenId)
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
import XMonad.Layout.NoBorders (smartBorders)
import XMonad.Layout.ResizableThreeColumns
  ( MirrorResize (MirrorExpand, MirrorShrink),
    ResizableThreeCol (ResizableThreeColMid),
  )
import XMonad.Layout.ResizableTile (ResizableTall (ResizableTall))
import XMonad.Layout.ThreeColumns (ThreeCol (ThreeColMid))
import XMonad.Prompt (XPConfig (font, height))
import XMonad.Prompt.ConfirmPrompt (confirmPrompt)
import XMonad.StackSet qualified as W
import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Util.NamedScratchpad (NamedScratchpad (NS), NamedScratchpads, customFloating, defaultFloating, namedScratchpadAction, namedScratchpadManageHook)
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

scratchpads :: NamedScratchpads
scratchpads =
  [ NS
      "xterm"
      "xterm -name scratch"
      (appName =? "scratch") -- position: 2/3 width from the left 1/2 height from the top, dimensions: 1/3 width by 1/2 height
      (customFloating $ W.RationalRect (2 / 3) (1 / 2) (1 / 3) (1 / 2)),
    NS
      "telegram"
      "telegram-desktop"
      (className =? "TelegramDesktop")
      defaultFloating,
    NS
      "keepassxc"
      "keepassxc"
      (className =? "KeePassXC")
      defaultFloating
  ]

-- This is needed to run scripts, because `spawn` uses `sh` which does not read
-- `.bashrc` and so does not know about any user defined `$PATH`.
myspawn x = void $ spawnPID x
  where
    spawnPID x = xfork $ executeFile "/usr/bin/env" False ["bash", "-c", x] Nothing

myKeys :: [(String, X ())]
myKeys =
  [ ("M-C-x", confirmPrompt myXPConfig "exit" $ io exitSuccess),
    ("M-S-q", kill), -- close focused window
    ("M-q", spawn "xmonad --restart"), -- don't recompile, nix does it for us
    ("M-S-<Return>", spawn myTerm), -- myTerm is appended by Nix
    ("M-S-h", sendMessage MirrorShrink), -- shrink slave size
    ("M-S-l", sendMessage MirrorExpand), -- expand slave size
    ("M-g", goToSelected myGsConfig),
    ("M-d", spawn "rofi -show combi"),
    ("M-o", namedScratchpadAction scratchpads "xterm"),
    ("M-C-t", namedScratchpadAction scratchpads "telegram"),
    ("M-C-k", namedScratchpadAction scratchpads "keepassxc"),
    ("M-<Tab>", toggleWS' ["NSP"]),
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
    -- M-Shift-[1-9] moves windows to workspaces
    -- M-Shift-[1-9] moves windows to workspaces
    -- M-Shift-[1-9] moves windows to workspaces
    -- M-[1-9] views workspaces
    -- M-[1-9] views workspaces
    -- M-[1-9] views workspaces
    -- M-[1-9] views workspaces

    -- M-Shift-[1-9] moves windows to workspaces
    -- M-[1-9] views workspaces
    [ ("M-" ++ mask ++ show key, windows $ onCurrentScreen f i)
      | (key, i) <- zip [1 .. 9] myWorkspaces,
        (f, mask) <- [(W.view, ""), (W.shift, "S-")]
    ]
    ++ [ ("<XF86AudioMute>", myspawn "changevolume toggle"),
         ("<XF86AudioRaiseVolume>", myspawn "changevolume inc"),
         ("<XF86AudioLowerVolume>", myspawn "changevolume dec")
       ]

-- @q =?~ x@. matches @q@ using the regex @x@, return 'True' if it matches
(=?~) :: Query String -> String -> Query Bool
q =?~ regex = fmap (matchRegex regex) q
  where
    matchRegex :: String -> String -> Bool
    matchRegex pattern string = string =~ pattern

myManageHook :: ManageHook
myManageHook =
  composeAll $
    [ className =? name --> doFloat
      | name <-
          [ "About",
            "Open Folder",
            "Picture in picture",
            "Picture-in-Picture",
            "net-runelite-launcher-Launcher",
            "Volume Control",
            "dialog",
            "plasmashell",
            "zoom"
          ]
    ]
      ++ [stringProperty "WM_WINDOW_ROLE" =? "pop-up" --> doFloat]
      ++ [className =? "plasmashell" <&&> isInProperty "_NET_WM_STATE" "_NET_WM_STATE_SKIP_TASKBAR" --> doIgnore]

myLogHook =
  xmobarPP
    { ppSep = " | ",
      ppCurrent = xmobarColor "green" "" . wrap "[" "]",
      ppVisible = xmobarColor "lightgreen" "" . wrap "[" "]",
      ppHidden = xmobarColor "gray" "" . wrap "(" ")",
      ppTitle = xmobarColor "cyan" "" . shorten 50, -- window title format
      ppSort = getSortByTag,
      ppOrder = \(ws : layout : wt : extra) -> [layout, ws, wt] ++ extra
    }

xmobarMain =
  statusBarPropTo
    "_XMONAD_LOG"
    "xmobar -x 0 ~/.config/xmobar/xmobarrc"
    (pure myLogHook)

barSpawner :: ScreenId -> IO StatusBarConfig
barSpawner 0 = pure xmobarMain
barSpawner _ = mempty

myLayoutHook = ResizableTall 1 (1 / 100) (1 / 2) [] ||| ResizableThreeColMid 1 (1 / 100) (30 / 100) []

myConfig nScreens =
  desktopConfig
    { terminal = myTerm,
      modMask = mod4Mask, -- meta key
      normalBorderColor = "#999999",
      focusedBorderColor = "#FF0000",
      borderWidth = 3,
      manageHook = myManageHook <+> namedScratchpadManageHook scratchpads,
      layoutHook = smartBorders $ refocusLastLayoutHook $ avoidStruts myLayoutHook,
      workspaces = withScreens nScreens myWorkspaces,
      handleEventHook = refocusLastWhen (return True) <+> handleEventHook def
    }
    `additionalKeysP` myKeys

main :: IO ()
main = countScreens >>= \nScreens -> xmonad $ ewmhFullscreen . ewmh . dynamicEasySBs barSpawner $ docks (myConfig nScreens)
