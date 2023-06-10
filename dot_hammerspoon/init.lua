hs.loadSpoon('SpoonInstall')

spoon.SpoonInstall:andUse('EmmyLua')
spoon.SpoonInstall:andUse('MouseFollowsFocus')

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

spoon.MouseFollowsFocus:start()

GOPASS_CHOOSER = hs.chooser.new(function(result)
  if result == nil then
    return
  end

  local target = result["text"]

  local _, status = hs.execute("gopass show --clip " .. target, true)

  if not status then
    hs.alert.show("error while copying password for `" .. target .. "`")
  end
end)

GOPASS_CHOOSER:choices(function()
  local output, status = hs.execute("gopass list -f", true)

  if not status or output == nil then
    return {}
  end

  local result = {}

  for s in output:gmatch("[^\r\n]+") do
    table.insert(result, {
      ["text"] = s
    })
  end

  return result
end)

hs.hotkey.bind(SUPER, 'p', function()
  if GOPASS_CHOOSER:isVisible() then
    return
  end

  GOPASS_CHOOSER:refreshChoicesCallback(true)

  GOPASS_CHOOSER:show()
end)
