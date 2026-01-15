local function extend_default_filetypes(lsp, ...)
  local filetypes = { ... }

  for _, ft in ipairs(vim.lsp.config[lsp].filetypes) do
    table.insert(filetypes, ft)
  end

  return filetypes
end

local function buf_set_keymaps(bufnr)
  -- Mappings.
  local opts = { noremap = true, silent = true, buffer = bufnr }

  local provideOpts = function(description)
    return vim.tbl_deep_extend("force", opts, { desc = description })
  end

  local diagnosticWrapper = function(c)
    return function()
      vim.diagnostic.jump(c)
    end
  end

  -- See `:help vim.lsp.*` for documentation on any of the below functions
  vim.keymap.set("n", "<space>wa", vim.lsp.buf.add_workspace_folder, provideOpts("Add workspace folder"))
  vim.keymap.set("n", "<space>wr", vim.lsp.buf.remove_workspace_folder, provideOpts("Remove workspace folder"))
  vim.keymap.set("n", "<space>wl", function()
    print(vim.inspect(vim.lsp.buf.list_workspace_folders()))
  end, provideOpts("List workspace folders"))
  vim.keymap.set("n", "<space>rn", vim.lsp.buf.rename, provideOpts("Rename"))
  vim.keymap.set({ "n", "v" }, "<space>a", vim.lsp.buf.code_action, provideOpts("Code actions"))

  vim.keymap.set("n", "[d", diagnosticWrapper({ count = -1, float = true }), provideOpts("Goto previos diagnostic"))
  vim.keymap.set("n", "]d", diagnosticWrapper({ count = 1, float = true }), provideOpts("Goto next diagnostic"))
  vim.keymap.set("n", "<leader>do", vim.diagnostic.open_float, provideOpts("Open floating diagnostic message"))
  vim.keymap.set("n", "<space>dl", vim.diagnostic.setloclist, provideOpts("Open diagnostics list"))
end

vim.api.nvim_create_autocmd("LspAttach", {
  group = vim.api.nvim_create_augroup("LspAttach_keymap", { clear = true }),
  callback = function(args)
    local bufnr = args.buf
    local client = vim.lsp.get_client_by_id(args.data.client_id)

    if client == nil then
      dd("got nil client")
      bt()
      return
    end

    if client.server_capabilities.definitionProvider then
      vim.bo[bufnr].tagfunc = "v:lua.vim.lsp.tagfunc"
    end

    if vim.lsp.inlay_hint then
      vim.lsp.inlay_hint.enable(true, { bufnr = bufnr })
    end

    buf_set_keymaps(bufnr)
  end,
})

return {
  {
    "williamboman/mason-lspconfig.nvim",

    dependencies = {
      { "mason-org/mason.nvim",             opts = {} },
      { "Hoffs/omnisharp-extended-lsp.nvim" },
      { "neovim/nvim-lspconfig" },
      { "nanotee/sqls.nvim" },
      { "ray-x/lsp_signature.nvim" },
      { "b0o/SchemaStore.nvim" },
      { "mrjones2014/codesettings.nvim" }
    },

    config = function()
      local mason_lspconfig = require("mason-lspconfig")

      vim.lsp.config('*', {
        before_init = function(_, config)
          local codesettings = require('codesettings')
          config = codesettings.with_local_settings(config.name, config)
        end,
      })

      vim.lsp.config("jsonls", {
        commands = {
          Format = {
            function()
              vim.lsp.buf.range_formatting({}, { 0, 0 }, { vim.fn.line("$"), 0 })
            end
          }
        },
        settings = {
          json = {
            schemas = require('schemastore').json.schemas(),
            validate = { enable = true },
          },
        },
      })

      vim.lsp.config('gopls', {
        init_options = {
          env = { GOFLAGS = "-tags=unit" },
          hints = {
            assignVariableTypes = true,
            compositeLiteralFields = true,
            constantValues = true,
            functionTypeParameters = true,
            parameterNames = false,
            rangeVariableTypes = true
          },
          usePlaceholders = true,
        },
      })

      vim.lsp.config("html", {
        filetypes = extend_default_filetypes("html", "templ"),
      })

      vim.lsp.config("htmx", {
        filetypes = extend_default_filetypes("htmx", "templ"),
      })

      if vim.fn.executable("sqls") == 1 then
        -- This is the best way I found of overriding on_attach, while still getting the config
        -- from sqls.
        local sqls_on_attach = vim.lsp.config["sqls"].on_attach

        vim.lsp.config("sqls", {
          on_attach = function(client, bufnr)
            if sqls_on_attach == nil then
              dd("got nil on_attach")
              bt()
              return
            end

            sqls_on_attach(client, bufnr)

            vim.keymap.set({ "n", "v" }, "<C-CR>", ":SqlsExecuteQuery<CR>",
              { silent = true, desc = "Execute query", buffer = bufnr })

            vim.keymap.set({ "n", "v" }, "<M-CR>", ":SqlsExecuteQueryVertical<CR>",
              { silent = true, desc = "Execute query", buffer = bufnr })

            vim.keymap.set("n", "<leader>qc", ":SqlsSwitchConnection<CR>",
              { silent = true, desc = "S[Q]L switch [C]onnection", buffer = bufnr })

            vim.keymap.set({ "n" }, "<leader>qd", ":SqlsSwitchDatabase<CR>",
              { silent = true, desc = "S[Q]L switch [D]atabase", buffer = bufnr })
          end
        })
        vim.lsp.enable("sqls")
      end

      if vim.fn.executable("rust-analyzer") == 1 then
        vim.lsp.config("rust_analyzer", {})
        vim.lsp.enable("rust_analyzer")
      end


      vim.lsp.config("yamlls", {
        settings = {
          yaml = {
            schemaStore = {
              -- You must disable built-in schemaStore support if you want to use
              -- this plugin and its advanced options like `ignore`.
              enable = false,
              -- Avoid TypeError: Cannot read properties of undefined (reading 'length')
              url = "",
            },
            schemas = require('schemastore').yaml.schemas(),
          },
        },
        redhat = {
          telemetry = {
            enabled = false,
          },
        },
      })

      vim.lsp.config("helm_ls", {
        settings = {
          logLevel = "info",
          valuesFiles = {
            mainValuesFile = "values.yaml",
            lintOverlayValuesFile = "values.lint.yaml",
            additionalValuesFilesGlobPattern = "values*.yaml"
          },
          yamlls = {
            enabled = true,
            diagnosticsLimit = 50,
            showDiagnosticsDirectly = false,
            path = "yaml-language-server",
            config = {
              schemas = {
                kubernetes = "templates/**",
              },
              completion = true,
              hover = true,
              -- any other config from https://github.com/redhat-developer/yaml-language-server#language-server-settings
            }
          }
        }
      })

      mason_lspconfig.setup()
    end,
  },

  {
    "neovim/nvim-lspconfig",

    dependencies = {
      -- { "folke/neoconf.nvim", opts = true, },
      { "mrjones2014/codesettings.nvim", opts = {} }
    },
  },

  {
    "j-hui/fidget.nvim",

    tag = "legacy",

    event = "LspAttach",

    opts = {
      window = {
        blend = 0,
      },
    }
  },

  {
    "folke/trouble.nvim",

    event = { "VeryLazy" },

    dependencies = {
      { "nvim-tree/nvim-web-devicons" }
    },

    opts = {
      fold_open = "v",      -- icon used for open folds
      fold_closed = ">",    -- icon used for closed folds
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
