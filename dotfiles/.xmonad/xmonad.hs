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
import           Graphics.X11.ExtraTypes
import           System.Directory               ( setCurrentDirectory
                                                , getHomeDirectory
                                                )

import           XMonad                  hiding ( (|||) )
import           XMonad.Layout.Spacing
import           XMonad.Layout.LayoutCombinators
                                                ( (|||)
                                                , JumpToLayout(..)
                                                )


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
                                                , checkKeymap
                                                , mkNamedKeymap
                                                , additionalKeysP
                                                , additionalKeys
                                                )

import           XMonad.Util.UnGrab              ( unGrab )
import           XMonad.Util.NamedActions
import           XMonad.Util.NamedScratchpad

import qualified XMonad.Util.ExtensibleState   as XS
import           XMonad.Hooks.FadeWindows
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
import           XMonad.Layout.ThreeColumns     ( ThreeCol (ThreeColMid) )
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


main = do
  xmonad
     . ewmhFullscreen
     . ewmh
     . dynamicEasySBs barSpawner
     $ overlayMyBaseSettings def
      `additionalKeysP`
      [ ("M-S-z", spawn "xscreensaver-command -lock")
      , ("M-C-s", unGrab >> spawn "scrot -s -o /dev/stdout | xclip -selection clipboard -t image/png -i $f")
      , ("M-g", spawn "google-chrome-stable --user-data-directory=/home/geoff/.config/google-chrome/Profile\\ 1/")
      , ("M-f", spawn "firefox")
      , ("M-p", spawn "rofi -show run")
      ]
      `additionalKeys`
        [ 
          
          ((0, xF86XK_AudioLowerVolume         ), spawn "amixer set Master 5%-")
        , ((0, xF86XK_AudioRaiseVolume         ), spawn "amixer set Master 5%+")
        , ((mod4Mask .|. shiftMask, xK_m        ), spawn "echo 'Hi, mom!' | dzen2 -p 4") ]

barSpawner :: ScreenId -> IO StatusBarConfig
barSpawner (S screen) = let n = show screen in pure $ statusBarPropTo ("_XMONAD_LOG") ("xmobar ~/.xmonad/xmobar.hs -x " ++ n) (pure myXmobarPP)

-- overlayKeys baseConfig =
--   addDescrKeys ((ctrlKey .|. winKey, xK_h), xMessage) myKeys baseConfig

overlayMyBaseSettings baseConfig = baseConfig
  { normalBorderColor  = myBlack
  , focusedBorderColor = myBlue
  , focusFollowsMouse  = True
  , layoutHook         = myLayouts
        -- Action to run when a new window is opened, <+> compoeses right to left
  , manageHook         = manageHook baseConfig <+> myManageHook
  , modMask            = mod4Mask
  , terminal           = myTerminal
  , borderWidth        = 3
  , startupHook        = startupHook baseConfig <+> myStartupHook
  , logHook            = fadeWindowsLogHook myFadeHook
  , handleEventHook    = fadeWindowsEventHook
  }

myFadeHook = composeAll [                 transparency 0.02
                        , isUnfocused --> transparency 0.0
                        ]
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

myTerminal = "kitty"

myStartupHook = do
  spawnOnce "/home/geoff/.xmonad/xmonad-start.sh"
--  spawnOnce "alacritty -T nixconfig --class nixconfig,alacritty -e sh -c \"cd nixconfig && nvim .\""
--  spawnOnce "alacritty -T currentProj --class currentProj,alacritty -e sh -c \"cd src/pdf && nix develop && nvim .\""
  spawnOnce "obsidian"
  spawnOnce "google-chrome-stable"
--  spawnOnce
--    "trayer --edge top --align right --SetDockType true --SetPartialStrut true --expand true --width 5 --height 18 --transparent true --tint 0x5f5f5f &"
  -- workaround for Java Swing/GUI apps not working

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
  , className =? "Google-chrome" --> doShift "1"
  , className =? "currentProj" --> doShift "3"
  , className =? "nixconfig" --> doShift "4"
  , className =? "Cypress" --> doShift "7"
  , className =? "obsidian" --> doShift "8"
  -- move transient windows like dialogs/alerts on top of their parents
  , transience'
  ]

myLayouts = spacing 18 $ tiled ||| Mirror tiled ||| Full 
  where
    tiled    = Tall nmaster delta ratio
    nmaster  = 1      -- Default number of windows in the master pane
    ratio    = 1/2    -- Default proportion of screen occupied by master pane
    delta    = 3/100  -- Percent of screen to increment by when resizing panes
