{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Main where

import           System.Exit                    ( exitSuccess )
import           System.IO                      ( hPutStrLn )
import           Data.Monoid
import           Data.Maybe
import           Data.List
import           Control.Monad
import qualified Data.Map                      as M
import           Text.Printf                    ( printf )
import           GHC.IO.Handle                  ( Handle )
import           Graphics.X11.Xrandr
import           System.Directory               ( setCurrentDirectory
                                                , getHomeDirectory
                                                )

import           XMonad                  hiding ( (|||) )
import           XMonad.Layout.LayoutCombinators
                                                ( (|||)
                                                , JumpToLayout(..)
                                                )

import           XMonad.StackSet         hiding ( workspaces )
import           XMonad.Util.SpawnOnce          ( spawnOnce )
import           XMonad.Util.Run                ( safeSpawn
                                                , unsafeSpawn
                                                , runInTerm
                                                , spawnPipe
                                                )
import           XMonad.Config.Gnome            ( gnomeConfig
                                                , gnomeRun
                                                )
import           XMonad.Config.Xfce             ( xfceConfig )
-- helper functions for parsing keymaps
import           XMonad.Util.EZConfig           ( mkKeymap
                                                , additionalKeysP
                                                , checkKeymap
                                                , mkNamedKeymap
                                                )
import           XMonad.Util.NamedActions
import           XMonad.Util.NamedScratchpad
import qualified XMonad.Util.ExtensibleState   as XS
import           XMonad.Hooks.StatusBar.PP      (filterOutWsPP)
import           XMonad.Hooks.StatusBar         (dynamicEasySBs, statusBarPropTo, statusBarProp, withEasySB, defToggleStrutsKey, StatusBarConfig)
import XMonad.Hooks.EwmhDesktops (ewmh, ewmhFullscreen)

import           XMonad.Prompt
import           XMonad.Prompt.Workspace        ( workspacePrompt )


import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.ManageHelpers     ( isDialog
                                                , doFullFloat
                                                , doCenterFloat
                                                , transience'
                                                )
import           XMonad.Hooks.ManageDocks       ( AvoidStruts
                                                , avoidStruts
                                                , ToggleStruts(..)
                                                , manageDocks
                                                )
import           XMonad.Hooks.UrgencyHook
import           XMonad.Hooks.SetWMName         ( setWMName ) -- workaround for Java Swing/GUI apps not working

import           XMonad.Layout.IndependentScreens
                                                ( countScreens )
import           XMonad.Layout.LayoutModifier   ( ModifiedLayout )
import           XMonad.Layout.NoBorders        ( noBorders
                                                , smartBorders
                                                )
import           XMonad.Layout.Fullscreen       ( fullscreenFull
                                                , fullscreenSupport
                                                )
import           XMonad.Layout.Grid             ( Grid(..) )
import           XMonad.Layout.TwoPane          ( TwoPane(..) )
import           XMonad.Layout.Tabbed           ( simpleTabbed )
import           XMonad.Layout.Renamed          ( renamed
                                                , Rename(Replace)
                                                )

import           XMonad.Actions.DynamicWorkspaceGroups
                                                ( addRawWSGroup
                                                , addWSGroup
                                                , promptWSGroupView
                                                , viewWSGroup
                                                )
import           XMonad.Actions.PhysicalScreens ( PhysicalScreen(..)
                                                , getScreen
                                                , horizontalScreenOrderer
                                                , viewScreen
                                                , sendToScreen
                                                )
import           XMonad.Actions.WindowGo        ( ifWindows
                                                , ifWindow
                                                )

-- This visual system consists of the following:
-- xmonad = a basic dynamic tiling windonw manager
-- xmobar = a simple statusbar on top of xmonad
-- dmenu = a simple program launcher that is started with win+p
-- trayer = an area next to xmobar where running apps like dropbox show clickable icons.

main = do

  xmonad
     . ewmhFullscreen
     . ewmh
     . dynamicEasySBs barSpawner
     . overlayKeys
     . overlayMyBaseSettings
     $ xfceConfig

barSpawner :: ScreenId -> IO StatusBarConfig
barSpawner (S screen) = let n = show screen in pure $ statusBarPropTo ("_XMONAD_LOG") ("xmobar ~/.xmonad/xmobar.hs -x " ++ n) (pure myXmobarPP)
-- barSpawner (S screen) = let n = show screen in pure $ statusBarPropTo ("_XMONAD_LOG_" ++ n) ("xmobar ~/.xmonad/xmobar.hs -x " ++ n) (pure myXmobarPP)

myXmobarPP2 :: PP
myXmobarPP2 = def
    { ppSep             = magenta " • "
    , ppTitleSanitize   = xmobarStrip
    , ppCurrent         = wrap " " "" . xmobarBorder "Top" "#8be9fd" 2
    , ppHidden          = white . wrap " " ""
    , ppHiddenNoWindows = lowWhite . wrap " " ""
    , ppUrgent          = red . wrap (yellow "!") (yellow "!")
    , ppOrder           = \[ws, l, _, wins] -> [ws, l, wins]
    -- , ppExtras          = [logTitles formatFocused formatUnfocused]
    }
  where
    formatFocused   = wrap (white    "[") (white    "]") . magenta . ppWindow
    formatUnfocused = wrap (lowWhite "[") (lowWhite "]") . blue    . ppWindow

    -- | Windows should have *some* title, which should not not exceed a
    -- sane length.
    ppWindow :: String -> String
    ppWindow = xmobarRaw . (\w -> if null w then "untitled" else w) . shorten 30

    blue, lowWhite, magenta, red, white, yellow :: String -> String
    magenta  = xmobarColor "#ff79c6" ""
    blue     = xmobarColor "#bd93f9" ""
    white    = xmobarColor "#f8f8f2" ""
    yellow   = xmobarColor "#f1fa8c" ""
    red      = xmobarColor "#ff5555" ""
    lowWhite = xmobarColor "#bbbbbb" ""

overlayKeys baseConfig =
  addDescrKeys ((ctrlKey .|. winKey, xK_h), xMessage) myKeys baseConfig

overlayMyBaseSettings baseConfig = baseConfig
  { normalBorderColor  = myBlack
  , focusedBorderColor = myBlue
  , focusFollowsMouse  = true
  , layoutHook         = myLayouts
        -- Action to run when a new window is opened, <+> compoeses right to left
  , manageHook         = manageHook baseConfig <+> myManageHook
  , modMask            = mod4Mask
  , terminal           = myTerminal
  , borderWidth        = 3
  , startupHook        = startupHook baseConfig <+> myStartupHook
  }

-- Define how xmonad-workspace-status is displayed.
-- Every bar has a textarea for displaying that.
myXmobarPP = filterOutWsPP [scratchpadWorkspaceTag] $ xmobarPP
  { ppCurrent = xmobarColor myBlue "" . wrap "[" "]"  -- currently focused workspace
  , ppTitle   = xmobarColor myBlue "" . shorten 80  -- title of currently focused program
  , ppHidden  = xmobarColor myGray "" . wrap "(" ")" -- hidden workspaces but with windows
  }

myBlack = "#000000"
myBlue = "#0080FF"
myGray = "gray"

winKey = mod4Mask
ctrlKey = controlMask

myTerminal = "alacritty"

myStartupHook = do
  spawnOnce "/home/geoff/.xmonad/xmonad-start.sh"
  spawnOnce
    "trayer --edge top --align right --SetDockType true --SetPartialStrut true --expand true --width 10 --height 18 --transparent true --tint 0x5f5f5f &"
  -- workaround for Java Swing/GUI apps not working
  setWMName "LG3D"

--myStartupHook baseConfig = do
--  checkKeymap baseConfig (fileShortcuts ++ appShortcuts ++ mediaShortcuts ++ xmonadShortcuts baseConfig)


-- Action to run when a new window is opened.
-- Use "xprop" terminal command to find out properties of running programs.
-- resource (also known as appName) is the first element in WM_CLASS(STRING)
-- className is the second element in WM_CLASS(STRING)
-- title is WM_NAME(STRING)
myManageHook = composeAll
  [ className =? "stalonetray" --> doIgnore
  -- , title =? "Microsoft Teams"  --> doIgnore
  -- , resource =? "microsoft teams - preview"  --> doIgnore
  , isDialog --> doCenterFloat
  , namedScratchpadManageHook myScratchPads
  -- move transient windows like dialogs/alerts on top of their parents
  , transience'
  ]

myLayouts =
  renamed [Replace "Layout-Full"]  Full
    -- ||| rename "Fullscreen" (noBorders Full) -- remove borders with single screen
    ||| renamed [Replace "Layout-Master"] (Tall oneMasterWindow incStepSizePercent masterColumnSizePercent)
    ||| TwoPane incStepSizePercent masterColumnSizePercent
 where -- default tiling algorithm partitions the screen into two panes
  oneMasterWindow         = 1
  incStepSizePercent      = 10 / 100
  masterColumnSizePercent = 70 / 100
myKeys baseConfig = concatMap
  ($ baseConfig)
  [ geoffShortCuts ]
-- Define additional keymappings in compact emacs-string-style:
-- M- mod/win
-- C- Ctrl
-- S- Shift
-- M1-M5 for mod1-mod5 (find out which is which with "xmodmap")
geoffShortCuts baseConfig = [subtitle "Geoff"] ++ mkNamedKeymap
   baseConfig
   [ (key, spawn' command)
   | (key, command) <-
  `additionalKeysP`
    [ ("M-S-z", spawn "xscreensaver-command -lock")
    , ("M-C-s", unGrab *> spawn "scrot -s -o /dev/stdout | xclip -selection clipboard -t image/png -i $f"        )
    , ("M-g"  , spawn "google-chrome-stable --user-data-directory=/home/geoff/.config/google-chrome/Profile\\ 1/"                   )
    , ("M-f"  , spawn "firefox"                   )
    , ("M-p"  , spawn "rofi -show run")
    ]

fileShortcuts baseConfig = [subtitle "Files"] ++ mkNamedKeymap
  baseConfig
  [ ("M-d " ++ key, spawn' command)
  | (key, command) <-
    [ ("t", "freeplane /home/mahene/Documents/todo.mm")
    , ("i", "freeplane /home/mahene/Documents/ideas.mm")
    , ("s", "freeplane /home/mahene/Documents/songs.mm")
    , ("d", "nautilus ~/Downloads/")
    , ("h", "nautilus ~/")
    , ("v", "nautilus ~/Videos/")
    , ("f", "nautilus ~/Photos/")
    , ("p", "nautilus ~/Documents/papers/")
    , ("m", "nautilus ~/Music/meditations")
    , ( "l"
      , "nautilus ~/Documents/read-lookup"
      ) -- lookup
    , ("o", "ranger") -- files
    ]
  ]

appShortcuts baseConfig = [subtitle "Apps"] ++ mkNamedKeymap
  baseConfig
  [ ("M-a " ++ key, addName description $ spawn command)
  | (key, description, command) <-
    [("S-s", "", "/opt/tor-browser_en-US/start-tor-browser")]
  ]

monitorShortcuts baseConfig = [subtitle "Monitor output"] ++ mkNamedKeymap
  baseConfig
  [ ("M-M1-m " ++ key, addName description $ spawn command)
  | (key, description, command) <-
    [ ("n", "Only on notebook display", notebookOnly)
    , ("a", "On all screens"          , all)
    ]
  ]
 where
  externDisplayLeft = "DP-3" -- external monitor
  externDisplayRight  = "HDMI-1" -- external monitor
  auto              = "auto" -- turn dipslay on and select highest resolution automatically
  off               = "off" -- turn Display off
  --xrandrTemplate ="xrandr --output %s --%s --primary --output %s --%s --left-of %s --output %s --%s --right-of %s --mode 1440x900"
  xrandrTemplate
    = "xrandr --output %s --%s --primary --output %s --%s --left-of %s --output %s --%s --right-of %s"
  xrandr (left, modeLeft, mid, modeMid, right, modeRight) =
    printf xrandrTemplate mid modeMid left modeLeft mid right modeRight mid

mediaShortcuts baseConfig = [subtitle "Media"] ++ mkNamedKeymap
  baseConfig
  [ (key, addName description $ spawn command)
  | (key, description, command) <-
    [ ("M-[" , "Emulate previous song key", "xdotool key XF86AudioPrev")
    , ("M-]" , "Emulate next song key"    , "xdotool key XF86AudioNext")
    , ("M-\\", "Emulate Play/Pause key"   , "xdotool key XF86AudioPlay")
    , ( "M-u"
      , "Settings Ubuntu"
      , "unity-control-center"
      )
-- Volume Control
    , ("<XF86AudioMute>", "Mute", "pactl set-sink-mute @DEFAULT_SINK@ toggle")
    , ( "<XF86AudioLowerVolume>"
      , "Lower volume"
      , "pactl set-sink-volume @DEFAULT_SINK@ -10%"
      )
    , ( "<XF86AudioRaiseVolume>"
      , "Increase volume"
      , "pactl set-sink-volume @DEFAULT_SINK@ +10%"
      )
-- Brightness Control
--, ("<XF86MonBrightnessUp>", "Brightness up", "lux -a 10%")
--, ("<XF86MonBrightnessDown>", "Brightness up", "lux -s 10%")
    ]
  ]

firefoxCmd :: String -> String
firefoxCmd profileName = firefoxCmd' "Firefox" profileName

firefoxCmd' :: String -> String -> String
firefoxCmd' className profileName =
  printf "firefox --class %s -new-instance -p \"%s\"" className profileName

-- Xmonad extensions
xmonadShortcuts baseConfig = [subtitle "Xmonad extensions"] ++ mkNamedKeymap
  baseConfig
     , ( "M-<Space> s"
       , addName "Single app Layout" . sendMessage $ JumpToLayout "Single"
       )
     , ( "M-<Space> f"
       , addName "Fullscreen Layout" . sendMessage $ JumpToLayout "Fullscreen"
       )
     , ( "M-<Space> m"
       , addName "Master Pane Layout" . sendMessage $ JumpToLayout "Master"
       )
     , ( "M-<Space> g"
       , addName "Grid Layout" . sendMessage $ JumpToLayout "Grid"
       )
  -- Layouts
     , ( "M-g"
       , addName "Go to workspace by name"
         $ workspacePrompt def (windows . lazyView)
       )
     ]
  ++
    -- Replacing greedyView with view
     [ ("M-" ++ otherModKey ++ [key], noName . windows $ action workspaceID)
     | (key, workspaceID) <- zip numberKeys workspaceIds
     , (otherModKey, action) <-
       [("", lazyView), ("C-", greedyView), ("S-", shift)]
     ]
  )
 where
  numberKeys       = "123456789"
  workspaceIds     = workspaces baseConfig
  restartXMonadCmd = "xmonad --recompile && xmonad --restart"
  -- Xfce specific
  rebootCmd        = "xfce4-session-logout --reboot"
  shutdownCmd      = "xfce4-session-logout --halt"
  hibernateCmd     = "xfce4-session-logout --hibernate"
  logoutCmd        = "xfce4-session-logout"
  sessionLockCmd   = "xflock4"

lazyView
  :: (Eq w, Eq sid)
  => w
  -> StackSet w lay win sid sd
  -> StackSet w lay win sid sd
lazyView workspaceId stackSet =
  if isVisible workspaceId stackSet then stackSet else view workspaceId stackSet

isVisible :: Eq w => w -> StackSet w lay win sid sd -> Bool
isVisible workspaceId stackSet =
  any ((workspaceId ==) . tag . workspace) (visible stackSet)


scratchpadShortcuts baseConfig = [subtitle "Scratchpads"] ++ mkNamedKeymap
  baseConfig
  (  [ ( "M1-b " ++ key
       , addName scratchpadName
         $ namedScratchpadAction myScratchPads scratchpadName
       )
     | (key, scratchpadName) <-
       [ ("w", "webScratch")
       , ( "s"
         , "separateScratch"
         )
-- , ("d", "devScratch")
       , ("m", "musicScratch")
       , ("t", "teamsScratch")
       ]
     ]
  ++ [ ( "M1-" ++ key
       , addName scratchpadName
         $ namedScratchpadAction myScratchPads scratchpadName
       )
     | (key, scratchpadName) <- -- "a" doesnt work
       [ ("w", "workScratch")
       , ("d", "devScratch")
       , ("p", "dbeaverScratch")
       ]
     ]
  ++ [ ( "M1-a " ++ key
       , addName scratchpadName
         $ namedScratchpadAction myScratchPads scratchpadName
       )
     | (key, scratchpadName) <-
       [ ("k", "keepassScratch")
       -- m doesn't work
       , ("a", "cmusScratch")
       , ("d", "discordScratch")
       , ("i", "ircScratch")
       , ("n", "matrixScratch")
       , ("l", "languageScratch")
       , ("e", "thunderbirdScratch")
       , ("j", "joplinScratch")
       , ("s", "signalScratch")
       , ("t", "todoScratch")
       ]
     ]
  )

-- Action to run when a new window is opened.
-- Use "xprop" terminal command to find out properties of running programs.
-- resource (also known as appName) is the first element in WM_CLASS(STRING)
-- className is the second element in WM_CLASS(STRING)
-- title is WM_NAME(STRING)
myScratchPads =
  [ NS "keepassScratch" "keepassxc" (resource =? "keepassxc") manageWindow
  , NS "cmusScratch"
       ("alacritty --class cmusplayer -e cmus")
       -- ("urxvt -name cmusplayer -e cmus")
       (resource =? "cmusplayer")
       manageWindow
  , NS "musicScratch"
       (firefoxCmd' "musicScratch" "music")
       (className =? "musicScratch")
       manageWindow
  , NS "webScratch"
       (firefoxCmd' "webScratch" "web")
       (className =? "webScratch")
       manageWindow
  , NS "workScratch"
       (firefoxCmd' "workScratch" "work")
       (className =? "workScratch")
       manageWindow
  , NS "devScratch"
       (firefoxCmd' "devScratch" "dev")
       (className =? "devScratch")
       manageWindow
  , NS "dbeaverScratch" "dbeaver" (resource =? "DBeaver") manageWindow
  , NS "terminalScratch"
       ("alacritty --class " ++ "terminalScratch")
       (resource =? "terminalScratch")
       manageWindow
  , NS "separateScratch"
       (firefoxCmd' "separateScratch" "separate")
       (className =? "separateScratch")
       manageWindow
  , NS "languageScratch"
       (firefoxCmd' "languageScratch" "language")
       (className =? "languageScratch")
       manageWindow
  , NS "thunderbirdScratch" "thunderbird" (resource =? "Mail") manageWindow
  , NS "joplinScratch" "joplin-desktop" (resource =? "joplin") manageWindow
  , NS "signalScratch" "signal-desktop" (resource =? "signal") manageWindow
  , NS "ircScratch" "hexchat" (resource =? "hexchat") manageWindow
  , NS "matrixScratch" "element-desktop" (resource =? "element") manageWindow
  , NS "discordScratch" "discord" (resource =? "discord") manageWindow
  , NS "teamsScratch"
       "teams"
       (resource =? "microsoft teams - preview")
       manageWindow
  , NS
    "todoScratch"
    "urxvt -name blogtodo -e bash -c 'vim ~/Programming/blob_of_code/doc/todo.md'"
    (resource =? "blogtodo")
    manageWindow
  ]
 where
  manageWindow = customFloating $ RationalRect l t w h
  h            = 0.95
  w            = 0.95
  t            = 0.99 - h
  l            = 0.985 - w
