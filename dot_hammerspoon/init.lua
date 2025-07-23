hs.loadSpoon('SpoonInstall')

spoon.SpoonInstall:andUse('EmmyLua')

local debouncer = require('debouncer')
local themeDetection = require('theme_detection')

local SUPER = { 'cmd', 'ctrl' }

hs.hotkey.bind(SUPER, 't', function()
  os.execute('open -n -b net.kovidgoyal.kitty')
end)

-- Kitty theme changes
themeDetection:addHandler(function(darkTheme)
  if darkTheme then
    -- Sadly the PATH is messed up and I am currently not feeling to investigate.
    os.execute('/opt/homebrew/bin/kitty +kitten themes --reload-in=all Tokyo Night Storm')
  else
    os.execute('/opt/homebrew/bin/kitty +kitten themes --reload-in=all Tokyo Night Day')
  end
end)

-- Force NVIM to update theme
themeDetection:addHandler(function(_)
  -- This works, as the nvim config has a special handler for SIGWINCH signals.
  os.execute('pkill -SIGWINCH -f nvim')
end)

local function fuzzyQuery(s, m)
  local s_index = 1
  local m_index = 1
  local match_start = nil

  while true do
    if s_index > s:len() or m_index > m:len() then
      return -1
    end
    local s_char = s:sub(s_index, s_index)
    local m_char = m:sub(m_index, m_index)
    if s_char == m_char then
      if match_start == nil then
        match_start = s_index
      end
      s_index = s_index + 1
      m_index = m_index + 1
      if m_index > m:len() then
        local match_end = s_index
        local s_match_length = match_end - match_start
        local score = m:len() / s_match_length
        return score
      end
    else
      s_index = s_index + 1
    end
  end
end

local function _fuzzy_filter(debounce_key, initial_choices, chooser, timeout)
  return function(query)
    debouncer:debounce(debounce_key, timeout, function()
      if query:len() == 0 then
        chooser:choices(initial_choices)
        return
      end
      local picked_choices = {}
      for _, j in pairs(initial_choices) do
        local fullText = (j["text"] .. (j["subText"] or '')):lower()
        local score = fuzzyQuery(fullText, query:lower())
        if score > 0 then
          j["fzf_score"] = score
          table.insert(picked_choices, j)
        end
      end
      local sort_func = function(a, b) return a["fzf_score"] > b["fzf_score"] end
      table.sort(picked_choices, sort_func)
      chooser:choices(picked_choices)
    end)
  end
end

local function get_password_choices()
  local output, status = hs.execute("gopass list -f", true)

  if not status or output == nil then
    return {}
  end

  local result = {}

  for s in output:gmatch("[^\r\n]+") do
    table.insert(result, {
      ["text"] = s,
    })
  end

  return result
end

local GOPASS_CHOOSER = hs.chooser.new(function(result)
  if result == nil then
    return
  end

  local target = result["text"]

  local _, status = hs.execute("gopass show --clip " .. target, true)

  if not status then
    hs.alert.show("error while copying password for `" .. target .. "`")
  end
end)

hs.hotkey.bind(SUPER, 'p', function()
  if GOPASS_CHOOSER:isVisible() then
    return
  end

  local choices = get_password_choices()

  GOPASS_CHOOSER:refreshChoicesCallback(true)

  GOPASS_CHOOSER:queryChangedCallback(_fuzzy_filter('gopass', choices, GOPASS_CHOOSER, 0.5))

  GOPASS_CHOOSER:choices(choices)
  GOPASS_CHOOSER:show()
end)

local GOPASS_OTP_CHOOSER = hs.chooser.new(function(result)
  if result == nil then
    return
  end

  local target = result["text"]

  local _, status = hs.execute("gopass otp --clip " .. target, true)

  if not status then
    hs.alert.show("error while copying OTP token for `" .. target .. "`")
  end
end)

hs.hotkey.bind({'cmd', 'ctrl', 'shift'}, 'p', function()
  if GOPASS_OTP_CHOOSER:isVisible() then
    return
  end

  local choices = get_password_choices()

  GOPASS_OTP_CHOOSER:refreshChoicesCallback(true)

  GOPASS_OTP_CHOOSER:queryChangedCallback(_fuzzy_filter('gopass', choices, GOPASS_OTP_CHOOSER, 0.5))

  GOPASS_OTP_CHOOSER:choices(choices)
  GOPASS_OTP_CHOOSER:show()
end)

local WINDOW_CHOOSER = hs.chooser.new(function(result)
  if result == nil then
    return
  end

  local win = result["window"]
  if win == nil then
    return
  end

  win:focus()
end)

local function get_windows()
  local windows = hs.window.allWindows()

  local result = {}

  for _, w in ipairs(windows) do
    table.insert(result, {
      ["text"] = w:application():title() .. " " .. w:title(),
      ["id"] = w:id(),
      ["window"] = w,
    })
  end

  return result
end

hs.hotkey.bind(SUPER, 'e', function()
  if WINDOW_CHOOSER:isVisible() then
    return
  end

  local choices = get_windows()

  WINDOW_CHOOSER:refreshChoicesCallback(true)

  WINDOW_CHOOSER:queryChangedCallback(_fuzzy_filter('windows', choices, WINDOW_CHOOSER, 0.15))

  WINDOW_CHOOSER:choices(choices)
  WINDOW_CHOOSER:show()
end)
