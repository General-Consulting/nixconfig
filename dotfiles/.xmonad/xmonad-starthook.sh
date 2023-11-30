# Avoid blue screen light at night
# if [ -z "$(pgrep fluxgui)" ] ; then
if [ -x "$(command -v fluxgui)" ] ; then
  fluxgui &
fi

# if [ -z "$(pgrep dropbox)" ] ; then
#if [ -x "$(command -v dropbox)" ] ; then
  #dropbox start &
#fi

if [ -x "$(command -v nextcloud)" ] ; then
  nextcloud &
fi

# Network manager GUI
# if [ -z "$(pgrep nm-applet)" ] ; then
if [ -x "$(command -v nm-applet)" ] ; then
  # nm-applet --sm-disable &
  nm-applet &
fi

# if [ -z "$(pgrep system-config-printer-applet)" ] ; then
if [ -x "$(command -v system-config-printer-applet)" ] ; then
  system-config-printer-applet &
fi

# On login, we unlock the ssh keychain so we're not prompted for
# passphrases later. We pipe /dev/null to ssh-add to make it realize
# it's not running in a terminal. Otherwise, it won't launch the prompt.
#
# If you don't use the ssh keychain you may not want this. Commented
# by default as it is assumed many users will not want this feature.

# export SSH_ASKPASS="/usr/bin/ssh-askpass"
# cat /dev/null | ssh-add &

# I disable the middle mouse button because otherwise I constantly
# accidentally paste unwanted text in the middle of my code while scrolling.
# Note that the id of the mouse device may be different depending on
# which usb port it is plugged into! To find it, use:
# xinput list |grep 'id='
# In the following command, the id is the first argument, the rest is
# the remapping.

# Commented by default as it is assumed many users will not want this.
# xinput set-button-map 10 1 0 3 4 5 6 7

# I disabled my touchpad because I hate those things. You can find the id
# of a device you want to disable using "xinput list"; unfortunately it can
# change depending on what devices you have plugged into USB. We extract the
# id of the device from the output of xinput, then use it to disable the
# device
# TOUCHPAD_ID=`xinput | grep 'Synaptics TouchPad' | cut -f 2 | cut -f 2 -d =`
# xinput set-prop $TOUCHPAD_ID "Device Enabled" 0
