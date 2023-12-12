-- big kudos to MariaSolOs as these two autocmds are stolen from her
vim.api.nvim_create_autocmd('FileType', {
    group = vim.api.nvim_create_augroup('patrickpichler/close_with_q', { clear = true }),
    desc = 'Close with <q>',
    pattern = {
        'help',
        'man',
        'scratch',
    },
    callback = function(args)
        vim.keymap.set('n', 'q', '<cmd>quit<cr>', { buffer = args.buf })
    end,
})

vim.api.nvim_create_autocmd('BufReadPost', {
    group = vim.api.nvim_create_augroup('patrickpichler/last_location', { clear = true }),
    desc = 'Go to the last location when opening a buffer',
    callback = function(args)
        local mark = vim.api.nvim_buf_get_mark(args.buf, '"')
        local line_count = vim.api.nvim_buf_line_count(args.buf)
        if mark[1] > 0 and mark[1] <= line_count then
            vim.cmd 'normal! g`"zz'
        end
    end,
})
