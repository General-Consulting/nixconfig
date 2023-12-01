Config
{ bgColor =      "white"
, fgColor =      "black"
, font =         "xft:Bitstream Vera Sans Mono:size=18:bold:antialias=true:hinting=true,Font Awesome 5 Free Solid:size=10:antialias=true:hinting=true"
--, additionalFonts = [ "xft:FontAwesome:pixelsize=13" ]
, position = TopW L 90
-- , border =       BottomB
-- , borderColor = "black"
-- general behavior
-- -- , lowerOnStart =     True    -- send to bottom of window stack on start
-- , hideOnStart =      False   -- start with window unmapped (hidden)
-- , allDesktops =      True    -- show on all desktops
-- , overrideRedirect = False -- run permanently as docked with XMonad
-- -- , pickBroadest =     False   -- choose widest display (multi-monitor)
-- -- , persistent =       True    -- enable/disable hiding (True = disabled)
 , iconRoot = "/home/mahene/.xmonad/icons/" 
, commands = -- what to display
  [ Run DynNetwork [ "-t" , "IO: <dev> <rx>|<tx>kBs"] 10
  , Run Wireless   "wlp3s0" ["-t", "\xf1eb [<qualitybar>] <essid>"] 20
  --, Run Network "wlp3s0" ["-t", "\xf0aa <rx>kb  \xf0ab <tx>kb"] 20
  -- , Run Weather "EDDK"
  --   [ "-t"," <tempC>C"
  --   , "-L","16","-H","24"
  --   , "--normal","green"
  --   , "--high","red"
  --   , "--low","lightblue"
  --   ] 36000
  , Run Cpu ["-t", "\xf108 (<total>%)","-H","90","--high","red"] 20
  , Run Memory [] 20
  --, Run Memory ["-t", "\xf538 (<total>%)","-H","90","--high","red"] 20
  , Run Battery
    [ "--template" , "<acstatus>"
    , "--Low"      , "10"        -- units: %
    , "--High"     , "80"        -- units: %
    , "--low"      , "darkred"
    , "--" -- battery specific options
    -- discharging status
    , "-o"	, "\xf241 <left>% (<timeleft>)"
    -- AC "on" status
    , "-O"	, "\xf0e7 <left>% (<timeleft>)"
    -- charged status
    , "-i"	, "\xf1e6"
    ] 20
  , Run Brightness ["-t", "\xf185 [<bar>]", "--", "-D", "amdgpu_bl0"] 10
  , Run Volume "default" "Master" ["-t", "\xf028 [<volumebar>] <status>"] 10
  -- , Run Alsa "default" "Master" [] % Not working yet
  , Run Date "%d %b %T" "mydate" 10
  -- -- , Run MPD ["-t", "<state>: <artist> - <track>"] 10
  , Run XMonadLog
  -- , Run StdinReader -- infos coming from xmonad
  ]
-- where to display command information along bar
-- , template = "%StdinReader% }{ %mpd% | %memory% | %bright% | %default:Master% | %dynnetwork% | %wlp4s0wi% | %battery% | %mydate% |"
, template = "%XMonadLog% }{ %cpu% | %memory% | %bright% | %default:Master% | %battery% | %mydate% |"
-- , template = "%XMonadLog% }{ "
}

