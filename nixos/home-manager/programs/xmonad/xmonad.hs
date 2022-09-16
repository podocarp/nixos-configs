{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

import Control.Monad (liftM2, void)
import qualified Data.Text as T
import System.Exit (exitSuccess)
import System.IO ()
import System.Posix.Process (executeFile)
import Text.Regex.Posix ((=~))
import XMonad
  ( Default (def),
    ManageHook,
    Query,
    ScreenId,
    Window,
    X,
    XConfig
      ( borderWidth,
        focusedBorderColor,
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
    doFloat,
    io,
    kill,
    mod4Mask,
    runQuery,
    sendMessage,
    stringProperty,
    title,
    windows,
    withWindowSet,
    xfork,
    xmonad,
    (-->),
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
import XMonad.Hooks.EwmhDesktops (ewmhFullscreen)
import XMonad.Hooks.ManageDocks ()
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
    onCurrentScreen,
    withScreens,
  )
import XMonad.Layout.ResizableThreeColumns
  ( MirrorResize (MirrorExpand, MirrorShrink),
    ResizableThreeCol (ResizableThreeColMid),
  )
import XMonad.Layout.ResizableTile (ResizableTall (ResizableTall))
import XMonad.Prompt (XPConfig (font, height))
import XMonad.Prompt.ConfirmPrompt (confirmPrompt)
import qualified XMonad.StackSet as W
import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Util.Run (spawnPipe)
import XMonad.Util.Scratchpad (scratchpadManageHook, scratchpadSpawnActionTerminal)
import XMonad.Util.WorkspaceCompare (getSortByTag)

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

spawn x = void $ spawnPID x
  where
    spawnPID x = xfork $ executeFile "/usr/bin/env" False ["bash", "-c", x] Nothing

myKeys :: [(String, X ())]
myKeys =
  [ ("M-S-c", confirmPrompt myXPConfig "exit" $ io exitSuccess),
    ("M-S-q", kill), -- close focused window
    ("M-S-<Return>", spawn myTerm), -- myTerm is appended by Nix
    ("M-S-h", sendMessage MirrorShrink), -- shrink slave size
    ("M-S-l", sendMessage MirrorExpand), -- expand slave size
    ("M-o", scratchpadSpawnActionTerminal myTerm),
    ("M-g", goToSelected myGsConfig),
    ("M-d", spawn "rofi -show combi"),
    ("M-f", spawn "rofi-pass"),
    ("M-p", spawn "autorandr -c"),
    ("M-<Tab>", toggleWS), -- exclude those on other screens
    -- screenshot and copies to clipboard
    ("<Print>", spawn "scrot -s -e 'xclip -selection clipboard -t image/png -i $f'")
  ]
    ++ [ ("M-" ++ mask ++ key, f scr)
         | (key, scr) <- zip ["w", "e", "r"] [0 ..],
           (f, mask) <- [(viewScreen horizontalScreenOrderer, ""), (sendToScreen horizontalScreenOrderer, "S-")]
       ]
    ++
    -- M-Shift-[1-9] moves windows to workspaces
    -- M-[1-9] greedyviews workspaces
    [ ("M-" ++ mask ++ show key, windows $ onCurrentScreen f i)
      | (key, i) <- zip [1 .. 9] myWorkspaces,
        (f, mask) <- [(W.greedyView, ""), (W.shift, "S-")]
    ]
    ++ [ ("<XF86AudioMute>", spawn "changevolume toggle"),
         ("<XF86AudioRaiseVolume>", spawn "changevolume 5%+"),
         ("<XF86AudioLowerVolume>", spawn "changevolume 5%-"),
         --("<XF86AudioMute>", spawn "echo -e 'sset Master toggle' | amixer -s")
         --, ("<XF86AudioRaiseVolume>", spawn "echo -e 'sset Master 5%+' | amixer -s")
         --, ("<XF86AudioLowerVolume>", spawn "echo -e 'sset Master 5%-' | amixer -s")
         -- Brightness controls
         ("<XF86MonBrightnessUp>", spawn "brightnessctl s 5%+"),
         ("<XF86MonBrightnessDown>", spawn "brightnessctl s 5%-")
       ]

scratchpadHook :: ManageHook
scratchpadHook = scratchpadManageHook (W.RationalRect l t w h)
  where
    h = 0.5 -- height
    w = 0.3 -- width
    t = 0.9 - h -- distance from top edge
    l = 1 - w -- distance from left edge

-- @q =?~ x@. matches @q@ using the regex @x@, return 'True' if it matches
(=?~) :: Query String -> String -> Query Bool
q =?~ regex = fmap (matchRegex regex) q
  where
    matchRegex :: String -> String -> Bool
    matchRegex pattern string = string =~ pattern

myManageHook :: ManageHook
myManageHook =
  composeAll $
    [ title =? name --> doFloat
      | name <-
          [ "Volume Control",
            "Open Folder"
          ]
    ]
      ++ [ title =?~ name --> doFloat
           | name <-
               [ ".?zoom.?" -- zoom modals
               ]
         ]
      ++ [ className =? name --> doFloat
           | name <-
               [ "About",
                 "Image Lounge", -- nomacs
                 "Picture in picture",
                 "Picture-in-Picture",
                 "TelegramDesktop",
                 "RuneLite Launcher",
                 "dialog"
               ]
         ]
      ++ [ stringProperty "WM_WINDOW_ROLE" =? name --> doFloat
           | name <-
               [ "pop-up"
               ]
         ]

-- This gives the hidden workspaces and the master window in those workspaces
-- as a string.
myExtras :: [X (Maybe String)]
myExtras = [withWindowSet (fmap safeUnpack . extraFormatting . getNames . W.hidden)]
  where
    -- Gets the master window's (if any) name in the workspace
    ripName (W.Workspace i _ (Just stack)) =
      liftM2 (\x y -> concat [header, x, dash, y]) c t
      where
        header = i ++ ":"
        dash = " - "
        fshorten :: X String -> X String
        fshorten = fmap (shorten 10)
        t = fshorten (runQuery title (W.focus stack))
        c = fshorten (runQuery className (W.focus stack))
    ripName _ = return ""

    -- Given a stack of workspaces, return a list of names as per above
    getNames ws = foldl (liftM2 (++)) (return "") (map ripName ws)
    extraFormatting = fmap (\s -> front ++ s ++ back)
      where
        front = "<fc=lightgray>"
        back = "</fc>"
    -- Gets the Maybe String out.
    safeUnpack s = if s == "" then Nothing else Just s

myLogHook =
  xmobarPP
    { ppSep = " | ",
      ppCurrent = xmobarColor "green" "" . wrap "[" "]",
      ppVisible = xmobarColor "lightgreen" "" . wrap "[" "]",
      ppHidden = xmobarColor "gray" "" . wrap "(" ")",
      ppTitle = xmobarColor "cyan" "" . shorten 100, -- window title format
      ppSort = getSortByTag,
      ppOrder = \(ws : layout : wt : extra) -> [layout, ws, wt] ++ extra
    }

myLogHookSecondary =
  myLogHook
    { ppTitle = xmobarColor "lightblue" "" . shorten 100
    }

xmobarMain =
  statusBarPropTo
    "_XMONAD_LOG"
    "xmobar -x 0 ~/.config/xmobar/xmobarrc"
    (pure myLogHook)

xmobarSecondary =
  statusBarPropTo
    "_XMONAD_LOG_1"
    "xmobar -x 1 ~/.config/xmobar/xmobarrc_unfocused"
    (pure myLogHookSecondary)

barSpawner :: ScreenId -> IO StatusBarConfig
barSpawner 0 = pure xmobarMain
barSpawner 1 = pure xmobarSecondary
barSpawner _ = mempty

myLayoutHook =
  ResizableTall 1 (1 / 100) (1 / 2) []
    ||| ResizableThreeColMid 1 (1 / 100) (30 / 100) []

main :: IO ()
main =
  xmonad . ewmhFullscreen . dynamicEasySBs barSpawner $
    desktopConfig
      { terminal = myTerm,
        modMask = mod4Mask, -- meta key
        normalBorderColor = "#999999",
        focusedBorderColor = "#FF0000",
        borderWidth = 5,
        manageHook = scratchpadHook <+> myManageHook,
        layoutHook = myLayoutHook,
        workspaces = withScreens 2 myWorkspaces
      }
      `additionalKeysP` myKeys
