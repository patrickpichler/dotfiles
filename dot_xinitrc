#!/bin/sh

userresources=$HOME/.Xresources
usermodmap=$HOME/.Xmodmap
sysresources=/etc/X11/xinit/.Xresources
sysmodmap=/etc/X11/xinit/.Xmodmap

# merge in defaults and keymaps

if [ -f $sysresources ]; then
    xrdb -merge $sysresources
fi

if [ -f $sysmodmap ]; then
    xmodmap $sysmodmap
fi

if [ -f "$userresources" ]; then
    xrdb -merge "$userresources"
fi

if [ -f "$usermodmap" ]; then
    xmodmap "$usermodmap"
fi

# start some nice programs

if [ -d /etc/X11/xinit/xinitrc.d ] ; then
 for f in /etc/X11/xinit/xinitrc.d/?*.sh ; do
  [ -x "$f" ] && . "$f"
 done
 unset f
fi

if [ -d $HOME/.config/xinit/xinitrc.d ] ; then
 for f in $HOME/.config/xinit/xinitrc.d/?*.sh ; do
  [ -x "$f" ] && . "$f"
 done
 unset f
fi

xrandr --auto

nitrogen --restore

if [  -n "$SCREEN_CONFIG"  ] && [ -x "$HOME/screenlayout/$SCREEN_CONFIG" ]; then
 . "$HOME/screenlayout/$SCREEN_CONFIG"
fi

xsettingsd &

xset -b

exec i3 -V >> ~/.local/logs/i3log 2>&1
