vim.filetype.add({
  pattern = {
    ['.*'] = {
      function(_, bufnr)
        local content = vim.api.nvim_buf_get_lines(bufnr, 0, 1, false)[1] or ''
        if vim.regex([[^#!\(/usr/bin/env \)\?bpftrace$]]):match_str(content) ~= nil then
          return 'bpftrace'
        end
      end,
    },
  },
  extension = {
    bpftrace = "bpftrace"
  },
})
