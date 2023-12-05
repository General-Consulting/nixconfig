myHDMI=HDMI-1
myDP=DP-3
xrandr --output $myHDMI --auto --primary
# If we find that a screen is connected, activate it and position it
# to the right of the primary screen.
xrandr | grep $myDP\ connected | ifne xrandr --output $myDP --auto --left-of $myHDMI

#debugging
xrandr >xrandr.output.log
