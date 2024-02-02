# Nix config

This repository contains a nix configuration and jank accompanying scripts to deploy them (and then update this repo on deployment).  It sets up an xmonad environment with some utilities for general development, and assumes there will be default.nix files in specific project dirs.  The machine is an un-modified Beelink GTR7 Ryzen 9 Pro 7940HS.

If you stumble across this repo, you might be interested in the dual monitor setup, particularly the `boot.kernelParams` to help keep both monitors active.

Anything you find in here should be taken with a massive grain of salt, and comments/feedback are welcome.

Some specific choices include:

 - alacritty
 - zsh
 - xmonad
     - with lightdm 


Working:
 - Dual 4k 60fps display:
    - hdmi 
    - ~~usb-c ->~~ display port
 - Bluetooth via usb dongle
 - Wifi
 - Sound and Video (slack, zoom etc)
 - Keyboard and Mouse remote control of external OSX machine as extended monitor using Barrier.

Not Tested:
 - fingerprint reader
 

![image](https://github.com/General-Consulting/nixconfig/assets/143022822/21d3e4c6-88e4-4436-8b43-f54d96a27893)
