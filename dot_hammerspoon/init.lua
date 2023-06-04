hs.loadSpoon('SpoonInstall')

spoon.SpoonInstall:andUse('EmmyLua')

SUPER = { 'cmd', 'ctrl' }

hs.hotkey.bind(SUPER, 't', function()
  os.execute('open -n -b org.alacritty')
end)
