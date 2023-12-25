local ssh_config = require('ssh')

-- Pull in the wezterm API
---@type wezterm
local wezterm = require 'wezterm'
local act = wezterm.action

wezterm.on('update-right-status', function(window)
  local name = window:active_key_table()
  if name then
    name = 'TABLE: ' .. name
  end
  window:set_right_status(name or '')
end)

local config = {}

if wezterm.config_builder then
  config = wezterm.config_builder()
end

ssh_config.infuse(config)

config.color_scheme = 'Catppuccin Mocha'

config.tab_bar_at_bottom = true

config.window_padding = {
  left = 5,
  right = 5,
  top = 5,
  bottom = 5,
}

config.font = wezterm.font('FiraCode Nerd Font Mono', { weight = 'Medium' })
config.font_size = 13
config.freetype_load_flags = 'NO_HINTING'

local function is_vim(pane)
  -- this is set by the plugin, and unset on ExitPre in Neovim
  return pane:get_user_vars().IS_NVIM == 'true'
end

local direction_keys = {
  Left = 'h',
  Down = 'j',
  Up = 'k',
  Right = 'l',
  -- reverse lookup
  h = 'Left',
  j = 'Down',
  k = 'Up',
  l = 'Right',
}

local function split_nav(resize_or_move, key)
  return {
    key = key,
    mods = resize_or_move == 'resize' and 'META|SHIFT' or 'CTRL|SHIFT',
    action = wezterm.action_callback(function(win, pane)
      if is_vim(pane) then
        -- pass the keys through to vim/nvim
        win:perform_action({
          SendKey = { key = key, mods = resize_or_move == 'resize' and 'META' or 'CTRL' },
        }, pane)
      else
        if resize_or_move == 'resize' then
          win:perform_action({ AdjustPaneSize = { direction_keys[key], 3 } }, pane)
        else
          win:perform_action({ ActivatePaneDirection = direction_keys[key] }, pane)
        end
      end
    end),
  }
end

config.leader = { key = 's', mods = 'CTRL' }

config.keys = {
  -- move between split panes
  split_nav('move', 'h'),
  split_nav('move', 'j'),
  split_nav('move', 'k'),
  split_nav('move', 'l'),
  -- resize panes
  split_nav('resize', 'h'),
  split_nav('resize', 'j'),
  split_nav('resize', 'k'),
  split_nav('resize', 'l'),

  -- tmux like movements
  -- { mods = 'LEADER',   key = 'h',    action = wezterm.action.ActivatePaneDirection('Left') },
  -- { mods = 'LEADER',   key = 'k',    action = wezterm.action.ActivatePaneDirection('Up') },
  -- { mods = 'LEADER',   key = 'j',    action = wezterm.action.ActivatePaneDirection('Down') },
  -- { mods = 'LEADER',   key = 'k',    action = wezterm.action.ActivatePaneDirection('Right') },

  {
    mods   = 'CTRL|SHIFT',
    key    = 's',
    action = wezterm.action.SplitVertical { domain = 'CurrentPaneDomain' }
  },
  {
    mods   = 'CTRL|SHIFT',
    key    = 'v',
    action = wezterm.action.SplitHorizontal { domain = 'CurrentPaneDomain' }
  },
  {
    mods   = 'CTRL|SHIFT',
    key    = 'd',
    action = wezterm.action.ShowDebugOverlay,
  },
}

return config
