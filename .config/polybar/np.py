#!/usr/bin/env python3

import gi

gi.require_version('Playerctl', '2.0')

from gi.repository import Playerctl, GLib

manager = Playerctl.PlayerManager()

def on_metadata(player, e, manager):
    if player.props.status == 'Playing':
        meta = player.props.metadata
        playing_info = u'{artist} - {title}'.format(artist=meta['xesam:artist'][0],title=meta['xesam:title'])

        print(playing_info, flush=True)
    else:
        # Print empty line if nothing is playing
        print('', flush=True)

def init_player(name):
    # choose if you want to manage the player based on the name
    player = Playerctl.Player.new_from_name(name)
    player.connect('metadata', on_metadata, manager)
    manager.manage_player(player)
    on_metadata(player, None, manager)

def on_name_appeared(manager, name):
    init_player(name)

manager.connect('name-appeared', on_name_appeared)

for name in manager.props.player_names:
    init_player(name)

main = GLib.MainLoop()
main.run()
