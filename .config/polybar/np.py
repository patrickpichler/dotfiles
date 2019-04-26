#!/usr/bin/env python3

import os

os.environ["PYTHONIOENCODING"] = "utf-8"

import gi
import sys
gi.require_version('Playerctl', '2.0')
from gi.repository import Playerctl, GLib  # noqa: E402

print('', flush=True)


def on_metadata(player, e):
    if player.props.status == 'Playing':
        meta = player.props.metadata
        playing_info = bytes(u'{artist} - {title}'.format( artist=meta['xesam:artist'][0],title=meta['xesam:title']), 'utf-8').decode('ascii', 'ignore')
        print(playing_info, flush=True)
    else:
        # Print empty line if nothing is playing
        print('', flush=True)

try:
    player = Playerctl.Player()

    player.connect('metadata', on_metadata)

    on_metadata(player, None)

    main = GLib.MainLoop()
    main.run()
except Exception as e:
    print('', flush=True)
