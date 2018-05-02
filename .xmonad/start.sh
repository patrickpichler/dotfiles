if [ -s .Xresources ]; then
  xrdb -merge .Xresources
fi

if [ -x "$(command -v start-pulseaudio-x11)" ]; then
  start-pulseaudio-x11 & 
fi

if [ -x "$(command -v nm-applet)" ]; then
  nm-applet --sm-disable &
fi

if [ -x "$(command -v udiskie)" ]; then
  # Start udiskie to handle media
  udiskie --smart-tray &
fi

if [ -x "$(command -v blueberry-tray)" ]; then
  blueberry-tray &
fi

if [ -x "$(command -v nitrogen)" ]; then
  nitrogen --restore &
fi


exec ppichler-xmonad
