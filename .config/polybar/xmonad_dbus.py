#!/usr/bin/env  python3

import dbus.service
import dbus
from gi.repository import GLib
from dbus.mainloop.glib import DBusGMainLoop                                                           
import re

import sys

DBusGMainLoop(set_as_default=True)        

def xmonad_update_handler(string):
    print(format_ws_string(string))
    # print(string)

def format_ws_string(string):
    return replace_active_ws(replace_visible_ws(string)) 

def replace_visible_ws(string):
    return re.sub(r'\ <(\w*)\> ', format_underline("ff0000", " \g<1> "), string)

def replace_active_ws(string):
    return re.sub(r' \[(\w*)\] ', format_underline("ffff00", format_bg("3f3f3f", " \g<1> ")), string)

def format_fg(color, string):
    return format("F", color, string)

def format_bg(color, string):
    return format("B", color, string)

def format_underline(color, string):
    return f'%{{u#{color} +u}}{string}%{{-u}}'

def format(char, color, string):
    return f'%{{{char}#{color}}}{string}%{{{char}-}}'


# label-active = " %name% "
# label-active-foreground = #ffffff
# label-active-background = #3f3f3f
# label-active-underline = #fba922

bus = dbus.SessionBus()

bus.add_signal_receiver(xmonad_update_handler,
                        path='/org/xmonad/Log',
                        dbus_interface='org.xmonad.Log',
                        signal_name='Update')

loop = GLib.MainLoop()
loop.run()
