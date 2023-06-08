hs.loadSpoon('SpoonInstall')

spoon.SpoonInstall:andUse('EmmyLua')

SUPER = { 'cmd', 'ctrl' }

hs.hotkey.bind('cmd', 'h', function()
end)

hs.hotkey.bind(SUPER, 't', function()
  os.execute('open -n -b org.alacritty')
end)

hs.hotkey.bind(SUPER, 'h', function()
  hs.window.focusedWindow():focusWindowWest(nil, true, true)
end)

hs.hotkey.bind(SUPER, 'l', function()
  hs.window.focusedWindow():focusWindowEast(nil, true, true)
end)

hs.hotkey.bind(SUPER, 'k', function()
  hs.window.focusedWindow():focusWindowNorth(nil, true, true)
end)

hs.hotkey.bind(SUPER, 'j', function()
  hs.window.focusedWindow():focusWindowSouth(nil, true, true)
end)
