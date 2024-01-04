#!/usr/bin/env bash
echo "very top"

# Load X resources (fixes some font issues)
# xrdb -merge .Xresources

# Start compositing to support transparency. You can omit this
# if you prefer not to use any transparency, but it is likely to cause
# ugly black fringing with some programs such as synapse that expect
# transparency support.
# xcompmgr -n &

# We handle our screen configuration using xrandr. You are likely to want to
# edit this section to match the specific demands of your setup. The below
# setup is for a Lenovo W520 laptop that sometimes has an external monitor
# hooked up via either VGA or DVI, positioned to the left of the laptop.
#
# Note that you can run xrandr at the command line to get a list of your
# connected screens and find out what names you should use to refer to them.

# Activate primary screen
myHDMI=HDMI-1
myDP=DP-1
xrandr --output $myHDMI --auto --primary

echo "after first xrandr"
# If we find that a screen is connected, activate it and position it
# to the right of the primary screen.
xrandr | grep $myDP\ connected | ifne xrandr --output $myDP --auto --left-of $myHDMI
#xrandr | grep $myDisplayport\ connected | ifne xrandr --output $myDisplayport --auto --right-of $myDisplay
xmodmap .Xmodmap
echo "after xmodmap"
# Restore backgrounds, set background with run 'feh --bg-scale /path/to/images'
#if [ -x $(command -v feh) ] ; then

feh --bg-scale ~/wallpapers/river-mountain.jpg &
echo "after feh"
picom -f &
echo "after picom"
# Line by line, the options used by default below mean:
# - icons should be aligned with the "East" or right side of the tray
# - the width of the tray should be 5 icons wide by one icon tall, and it
#   should be located 0 pixels from the right of the screen (-0) and 0 pixels
#   from the top of the screen (+0).
# - By setting our maximum geometry to the same thing, the tray will not grow.
# - The background color of the tray should be black.
# - This program should not show up in any taskbar.
# - Icons should be set to size "12".
# - Kludges argument of "force_icons_size" forces all icons to really, truly
#   be the size we set.
# - window-strut "none" means windows are allowed to cover the tray. In
#   other words, trust xmonad to handle this part.
#
if [ -x $(command -v stalonetray) ]; then
	touch ~/.stalonetrayrc # workaround: Stalonetray shows warning
	stalonetray \
		--icon-gravity E \
		--geometry 7x1-0+0 \
		--max-geometry 7x1-0+0 \
		--background '#ffffff' \
		--skip-taskbar \
		--icon-size 12 \
		--kludges force_icons_size \
		--window-strut none \
		&
fi

# Run the gnome-keyring-daemon to avoid issues you otherwise may encounter
# when using gnome applications which expect access to the keyring, such
# as Empathy. This prevents prompts you may otherwise get for invalid
# certificates and the like.
#gnome-keyring-daemon --start --components=gpg,pkcs11,secrets,ssh
# gnome-keyring-daemon --start

#gnome-settings-daemon -- does not start

# save battery
# gnome-power-manager

# Remap caps lock to left control. This is not strictly speaking
# xmonad related, but it's handy if you're a vim user.
# setxkbmap -option 'ctrl:nocaps'

# Change the default X cursor away from the "X"
#xsetroot -cursor_name left_ptr

#export GTK2_RC_FILES="$HOME/.gtkrc-2.0"

# /home/geoff/.dotfiles/xmonad/xmonad-starthook.sh

# Now, finally, start xmonad
#exec xmonad
