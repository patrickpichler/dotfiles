start-pulseaudio-x11 & 

nm-applet --sm-disable &

 # Start udiskie to handle media
udiskie --smart-tray &

blueberry-tray &

nitrogen --restore &

jetbrains-toolbox --minimize &

exec xmonad
