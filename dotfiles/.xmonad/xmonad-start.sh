#!/usr/bin/env bash
myHDMI=HDMI-1
myDP=DP-1
xrandr --output $myHDMI --auto --primary

xrandr | grep $myDP\ connected | ifne xrandr --output $myDP --auto --left-of $myHDMI
xmodmap .Xmodmap

feh --bg-scale ~/wallpapers/river-mountain.jpg &
picom -f &

if [ -x $(command -v stalonetray) ]; then
	touch ~/.stalonetrayrc # workaround: Stalonetray shows warning
	stalonetray \
		--icon-gravity E \
		--geometry 7x1-0+0 \
		--max-geometry 7x1-0+0 \
		--background '#ffffff' \
		--skip-taskbar \
		--icon-size 36 \
		--kludges force_icons_size \
		--window-strut none \
		&
fi

flameshot
