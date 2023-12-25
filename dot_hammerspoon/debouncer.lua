local M = {
  _tasks = {},
}

function M:debounce(key, timeout, task)
  local existing_task = self._tasks[key]

  if existing_task and existing_task:running() then
    existing_task:stop()
  end

  local timer = hs.timer.doAfter(timeout, task)

  self._tasks[key] = timer
end

return M
