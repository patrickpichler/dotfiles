start-pulseaudio-x11 & 

# stalonetray &
nm-applet --sm-disable &

 # Start udiskie to handle media
udiskie --smart-tray &

blueberry-tray &

sh /home/patrick/.fehbg

exec xmonad
