hs.loadSpoon('SpoonInstall')

spoon.SpoonInstall:andUse('EmmyLua')

SUPER = { 'cmd', 'ctrl' }

hs.hotkey.bind(SUPER, 't', function()
  os.execute('open -n -b org.alacritty')
end)

hs.hotkey.bind(SUPER, 'h', function()
  hs.window.filter.defaultCurrentSpace:focusWindowWest(nil,true,true)
end)

hs.hotkey.bind(SUPER, 'l', function()
  hs.window.filter.defaultCurrentSpace:focusWindowEast(nil,true,true)
end)

hs.hotkey.bind(SUPER, 'j', function()
  hs.window.filter.defaultCurrentSpace:focusWindowNorth(nil,true,true)
end)

hs.hotkey.bind(SUPER, 'k', function()
  hs.window.filter.defaultCurrentSpace:focusWindowSouth(nil,true,true)
end)
