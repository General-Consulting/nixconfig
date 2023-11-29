import XMonad
import XMonad.Operations
import XMonad.Layout.Magnifier
import XMonad.Util.EZConfig
import XMonad.Util.SpawnOnce
import XMonad.Util.Ungrab
import XMonad.Hooks.EwmhDesktops
import Graphics.X11.ExtraTypes.XF86
import Data.Bits ((.|.))
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP



main :: IO ()
main = xmonad $ ewmhFullscreen $ ewmh $ myXmobarProp $ myConfig

myConfig = def
    { 
      terminal   = "alacritty"
    , modMask    = mod4Mask  -- Rebind Mod to the Super key
    , layoutHook = myLayout  -- Use custom layouts
    }
  `additionalKeysP`
    [ ("M-S-z", spawn "xscreensaver-command -lock")
    , ("M-C-s", unGrab *> spawn "scrot -s -o /dev/stdout | xclip -selection clipboard -t image/png -i $f"        )
    , ("M-g"  , spawn "google-chrome-stable --user-data-directory=/home/geoff/.config/google-chrome/Profile\\ 1/"                   )
    , ("M-f"  , spawn "firefox"                   )
    , ("M-p"  , spawn "rofi -show run")
    ] 
  `additionalKeys`
    [ 
      ((0, xF86XK_AudioLowerVolume         ), spawn "amixer set Master 5%-")
    , ((0, xF86XK_AudioRaiseVolume         ), spawn "amixer set Master 5%+")
    , ((mod4Mask .|. shiftMask, xK_m        ), spawn "echo 'Hi, mom!' | dzen2 -p 4") ]


myLayout = tiled ||| Mirror tiled ||| Full ||| threeCol
  where
    tiled    = Tall nmaster delta ratio
    nmaster  = 1      -- Default number of windows in the master pane
    ratio    = 1/2    -- Default proportion of screen occupied by master pane
    delta    = 3/100  -- Percent of screen to increment by when resizing panes


xmobarConfig = Config { overrideRedirect = False
       , font     = "xft:iosevka-9"
       , bgColor  = "#5f5f5f"
       , fgColor  = "#f8f8f2"
       , position = TopW L 90
       , commands = [ Run Weather "EGPF"
                        [ "--template", "<weather> <tempC>°C"
                        , "-L", "0"
                        , "-H", "25"
                        , "--low"   , "lightblue"
                        , "--normal", "#f8f8f2"
                        , "--high"  , "red"
                        ] 36000
                    , Run Cpu
                        [ "-L", "3"
                        , "-H", "50"
                        , "--high"  , "red"
                        , "--normal", "green"
                        ] 10
                    , Run Alsa "default" "Master"
                        [ "--template", "<volumestatus>"
                        , "--suffix"  , "True"
                        , "--"
                        , "--on", ""
                        ]
                    , Run Memory ["--template", "Mem: <usedratio>%"] 10
                    , Run Swap [] 10
                    , Run Date "%a %Y-%m-%d <fc=#8be9fd>%H:%M</fc>" "date" 10
                    , Run XMonadLog
                    ]
       , sepChar  = "%"
       , alignSep = "}{"
       , template = "%XMonadLog% }{ %alsa:default:Master% | %cpu% | %memory% * %swap% | %EGPF% | %date% "
       }

myXmobarProp config =
  withEasySB (statusBarProp "xmobar" (pure xmobarPP)) toggleStrutsKey config

myXmobarPP :: PP
myXmobarPP = def
