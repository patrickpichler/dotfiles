vim.lsp.handlers["textDocument/publishDiagnostics"] = vim.lsp.with(
  vim.lsp.diagnostic.on_publish_diagnostics, {
    update_in_insert = true,
  }
)

local function get_selected_range(bufnr)
  local startPos = vim.api.nvim_buf_get_mark(bufnr, '<')
  local endPos = vim.api.nvim_buf_get_mark(bufnr, '>')

  return { start = startPos, ['end'] = endPos }
end

local function buf_set_keymaps(_, bufnr)
  -- Mappings.
  local opts = { noremap = true, silent = true, buffer = bufnr }

  local telescopeBuiltin = require('telescope.builtin')

  -- See `:help vim.lsp.*` for documentation on any of the below functions
  vim.keymap.set('n', 'gD', vim.lsp.buf.declaration, opts)
  vim.keymap.set('n', 'gd', telescopeBuiltin.lsp_definitions, opts)
  vim.keymap.set('n', 'K', vim.lsp.buf.hover, opts)
  vim.keymap.set('n', 'gi', telescopeBuiltin.lsp_implementations, opts)
  vim.keymap.set('i', '<C-k>', vim.lsp.buf.signature_help, opts)
  vim.keymap.set('n', '<space>wa', vim.lsp.buf.add_workspace_folder, opts)
  vim.keymap.set('n', '<space>ws', telescopeBuiltin.lsp_dynamic_workspace_symbols, opts)
  vim.keymap.set('n', '<space>wd', telescopeBuiltin.diagnostics, opts)
  vim.keymap.set('n', '<space>s', telescopeBuiltin.lsp_document_symbols, opts)
  vim.keymap.set('n', '<space>wr', vim.lsp.buf.remove_workspace_folder, opts)
  vim.keymap.set('n', '<space>wl', function()
    print(vim.inspect(vim.lsp.buf.list_workspace_folders()))
  end, opts)
  vim.keymap.set('n', '<space>d', telescopeBuiltin.lsp_type_definitions, opts)
  vim.keymap.set('n', '<space>rn', vim.lsp.buf.rename, opts)
  vim.keymap.set('n', '<space>a', vim.lsp.buf.code_action, opts)
  vim.keymap.set('v', '<space>a', function()
      vim.lsp.buf.code_action { range = get_selected_range(bufnr) }
    end,
    opts)
  vim.keymap.set('n', 'gr', telescopeBuiltin.lsp_references, opts)
  vim.keymap.set('n', '<space>e', telescopeBuiltin.diagnostics, opts)
  vim.keymap.set('n', '[d', vim.diagnostic.goto_prev, opts)
  vim.keymap.set('n', ']d', vim.diagnostic.goto_next, opts)
  vim.keymap.set('n', '<space>q', vim.diagnostic.setloclist, opts)
  vim.keymap.set('n', '<space>f', function()
    vim.lsp.buf.format { async = true }
  end, opts)
  vim.keymap.set('v', '<space>f',
    function()
      vim.lsp.buf.format { async = true, range = get_selected_range(bufnr) }
    end, opts)
end

vim.api.nvim_create_autocmd("LspAttach", {
  group = vim.api.nvim_create_augroup("LspAttach_keymap", { clear = true }),
  callback = function(args)
    local bufnr = args.buf
    local client = vim.lsp.get_client_by_id(args.data.client_id)

    if client.server_capabilities.completionProvider then
      vim.bo[bufnr].omnifunc = "v:lua.vim.lsp.omnifunc"
    end
    if client.server_capabilities.definitionProvider then
      vim.bo[bufnr].tagfunc = "v:lua.vim.lsp.tagfunc"
    end

    buf_set_keymaps(bufnr)
  end,
})

return {
  {
    'williamboman/mason.nvim',
    version = 'v1.*',
    build = ":MasonUpdate",
    opts = true
  },

  {
    'williamboman/mason-lspconfig.nvim',

    version = 'v1.*',

    dependencies = {
      { 'hrsh7th/cmp-nvim-lsp' },
      { 'Hoffs/omnisharp-extended-lsp.nvim' },
      { 'neovim/nvim-lspconfig' },
      -- { 'nvim-telescope/telescope.nvim' },
    },

    config = function()
      local mason_lspconfig = require('mason-lspconfig')

      mason_lspconfig.setup({})

      local default_capabilities = vim.tbl_deep_extend(
        'force',
        vim.lsp.protocol.make_client_capabilities(),
        require('cmp_nvim_lsp').default_capabilities(),
        {
          textDocument = {
            foldingRange = {
              dynamicRegistration = false,
              lineFoldingOnly = true,
            },
          },
        }
      )

      mason_lspconfig.setup_handlers {
        function(server_name)
          require('lspconfig')[server_name].setup {
            capabilities = default_capabilities,
          }
        end,

        ['efm'] = function()
          require('lspconfig').efm.setup {
            capabilities = default_capabilities,
            filetypes = { 'python' },
          }
        end,

        ['omnisharp'] = function()
          require('lspconfig').omnisharp.setup {
            capabilities = default_capabilities,
            handlers = {
              ["textDocument/definition"] = require('omnisharp_extended').handler
            },
          }
        end,

        ['jsonls'] = function()
          require('lspconfig').jsonls.setup {
            capabilities = default_capabilities,
            commands = {
              Format = {
                function()
                  vim.lsp.buf.range_formatting({}, { 0, 0 }, { vim.fn.line("$"), 0 })
                end
              }
            }
          }
        end,

        ['gopls'] = function()
          require('lspconfig').gopls.setup {
            capabilities = default_capabilities,
            init_options = {
              env = { GOFLAGS = '-tags=unit' },
              hints = {
                assignVariableTypes = true,
                compositeLiteralFields = true,
                constantValues = true,
                functionTypeParameters = true,
                parameterNames = true,
                rangeVariableTypes = true
              }
            }
          }
        end,
      }
    end,
  },

  {
    'neovim/nvim-lspconfig',

    dependencies = {
      { "folke/neodev.nvim", opts = true, }
    },
  },

  {
    'j-hui/fidget.nvim',

    event = "LspAttach",

    opts = {
      window = {
        blend = 0,
      },
    }
  },

  {
    'lvimuser/lsp-inlayhints.nvim',

    config = function(_, opts)
      require('lsp-inlayhints').setup(opts)

      vim.api.nvim_create_autocmd('LspAttach', {
        group = vim.api.nvim_create_augroup('LspAttach_inlayhints', {}),
        callback = function(args)
          if not (args.data and args.data.client_id) then
            return
          end

          local bufnr = args.buf
          local client = vim.lsp.get_client_by_id(args.data.client_id)
          require('lsp-inlayhints').on_attach(client, bufnr)
        end,
      })
    end
  },

  {
    "folke/trouble.nvim",

    event = { 'VeryLazy' },

    dependencies = {
      { "nvim-tree/nvim-web-devicons" }
    },

    opts = {
      icons = true,
      fold_open = "v",    -- icon used for open folds
      fold_closed = ">",  -- icon used for closed folds
      indent_lines = false, -- add an indent guide below the fold icons
      signs = {
        -- icons / text used for a diagnostic
        error = "error",
        warning = "warn",
        hint = "hint",
        information = "info"
      },
      use_diagnostic_signs = true -- enabling this will use the signs defined in your lsp clien
    }
  }
}
