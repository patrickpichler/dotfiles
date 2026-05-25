local whitelist = {
  ["Finder"] = true,
  -- TODO(patrick.pichler): figure out why ghostty remap is not working in ghostty conf.
  -- ["Ghostty"] = true,
}

function isAltTabSwitcherVisibleAX()
  local app = hs.application.get("com.lwouis.alt-tab-macos")
  if not app then return false end

  local axApp = hs.axuielement.applicationElement(app)
  if not axApp then return false end

  local windows = axApp:attributeValue("AXWindows") or {}
  for _, w in ipairs(windows) do
    -- AltTab's thumbnail window will be present here when active
    local role = w:attributeValue("AXRole")
    local subrole = w:attributeValue("AXSubrole")
    if role == "AXWindow" then
      return true, { role = role, subrole = subrole }
    end
  end
  return false
end

local blocker = hs.eventtap.new({ hs.eventtap.event.types.keyDown }, function(event)
  local flags = event:getFlags()
  local keyCode = event:getKeyCode()
  -- keyCode 4 is "h"
  if keyCode == 4 and flags.cmd and not flags.shift and not flags.alt and not flags.ctrl then
    local app = hs.application.frontmostApplication():name()
    if not whitelist[app] and not isAltTabSwitcherVisibleAX() then
      return true -- swallow the event
    end
  end
  return false
end)
blocker:start()
