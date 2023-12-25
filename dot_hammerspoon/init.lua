hs.loadSpoon('SpoonInstall')

spoon.SpoonInstall:andUse('EmmyLua')

local debouncer = require('debouncer')

local SUPER = { 'cmd', 'ctrl' }

hs.hotkey.bind(SUPER, 't', function()
  os.execute('open -n -b com.github.wez.wezterm --args start --always-new-process')
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

local GOPASS_DEBOUNCER_KEY = 'gopass'

local function _fuzzy_filter(initial_choices, chooser)
  return function(query)
    debouncer:debounce(GOPASS_DEBOUNCER_KEY, 0.5, function()
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

local function get_choices()
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

hs.hotkey.bind(SUPER, 'p', function()
  if GOPASS_CHOOSER:isVisible() then
    return
  end

  local choices = get_choices()

  GOPASS_CHOOSER:refreshChoicesCallback(true)

  GOPASS_CHOOSER:queryChangedCallback(_fuzzy_filter(choices, GOPASS_CHOOSER))

  GOPASS_CHOOSER:choices(choices)
  GOPASS_CHOOSER:show()
end)
