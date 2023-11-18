import XMonad
import XMonad.Operations
import XMonad.Layout.Magnifier
import XMonad.Util.EZConfig
import XMonad.Util.SpawnOnce
import XMonad.Util.Ungrab
import XMonad.Hooks.EwmhDesktops
import XMonad.Layout.ThreeColumns
import Graphics.X11.ExtraTypes.XF86
import Data.Bits ((.|.))



main :: IO ()
main = xmonad $ ewmhFullscreen $ ewmh $ myConfig

myConfig = def
    { 
      terminal   = "alacritty"
    , modMask    = mod4Mask  -- Rebind Mod to the Super key
    , layoutHook = myLayout  -- Use custom layouts
    }
  `additionalKeysP`
    [ ("M-S-z", spawn "xscreensaver-command -lock")
    , ("M-C-s", unGrab *> spawn "scrot -s"        )
    , ("M-g"  , spawn "google-chrome-stable --profile-directory=/home/geoff/.config/google-chrome/Default/"                   )
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
    threeCol = magnifiercz' 1.3 $ ThreeColMid nmaster delta ratio
    tiled    = Tall nmaster delta ratio
    nmaster  = 1      -- Default number of windows in the master pane
    ratio    = 1/2    -- Default proportion of screen occupied by master pane
    delta    = 3/100  -- Percent of screen to increment by when resizing panes



myStartupHook :: X ()
myStartupHook = do
--spawnOnce "trayer --edge top --align right --SetDockType true \
--          \--SetPartialStrut true --expand true --width 10 \
--          \--transparent true --tint 0x5f5f5f --height 18"
--spawnOnce "feh --bg-fill --no-fehbg ~/.wallpapers/haskell-red-noise.png"

  spawnOnce "google-chrome"
  spawnOnce "xmodmap ~/.Xmodmap"
