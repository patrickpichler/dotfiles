return {
  { 'echasnovski/mini.ai',      version = '*', config = true },
  { 'echasnovski/mini.align',   version = '*', config = true },
  { 'echasnovski/mini.basics',  version = '*', config = true },
  { 'echasnovski/mini.comment', version = '*', config = true },
  { 'echasnovski/mini.pairs',   version = '*', config = true },
  {
    'echasnovski/mini.surround',
    version = '*',
    opts = {
      mappings = {
        add = "gza",
        delete = "gzd",
        find = "gzf",
        find_left = "gzF",
        highlight = "gzh",
        replace = "gzr",
        update_n_lines = "gzn",
      },
    },
  },

  {
    'echasnovski/mini.bracketed',
    version = '*',
    opts = {
      yank = { suffix = "" },
      treesitter = { suffix = "n" },
    }
  },

  { 'cappyzawa/trim.nvim', config = true },
  {
    'RRethy/vim-illuminate',

    event = { 'VeryLazy' },

    opts = {
      filetypes_denylist = {
        'neo-tree',
        'fugitive',
        'mason',
        'Trouble',
        'notify',
        'help',
        'Outline',
        'TelescopePrompt',
      },
    },
    config = function(_, opts)
      require('illuminate').configure(opts)

      vim.api.nvim_create_autocmd("LspAttach", {
        group = vim.api.nvim_create_augroup("LspAttach_illuminate", {}),
        callback = function(args)
          if not (args.data and args.data.client_id) then
            return
          end

          local client = vim.lsp.get_client_by_id(args.data.client_id)
          require('illuminate').on_attach(client)
        end
      })
    end,
  },

  { 'mattn/emmet-vim', },
  { 'vim-utils/vim-line', },
  {
    'kana/vim-textobj-entire',
    dependencies = {
      { 'kana/vim-textobj-user', },
    },
  },


  {
    'lukas-reineke/indent-blankline.nvim',

    event = { 'VeryLazy' },

    opts = {
      show_current_context = true,
    }
  },

  {
    'stevearc/aerial.nvim',

    event = { 'VeryLazy' },

    opts = {
      backends = { 'lsp', 'treesitter', 'markdown' },
      filter_kind = false,
      highlight_closest = false,
    },

    dependencies = {
      "nvim-treesitter/nvim-treesitter",
      "nvim-tree/nvim-web-devicons"
    },

    config = function(_, opts)
      require('aerial').setup(opts)

      vim.keymap.set('n', '<leader>at', function()
        vim.cmd(':AerialToggle!')
      end)
    end
  },

  {
    'SmiteshP/nvim-navic',
    opts = {
      icons = {
        File = ' ',
        Module = ' ',
        Namespace = ' ',
        Package = ' ',
        Class = ' ',
        Method = ' ',
        Property = ' ',
        Field = ' ',
        Constructor = ' ',
        Enum = ' ',
        Interface = ' ',
        Function = ' ',
        Variable = ' ',
        Constant = ' ',
        String = ' ',
        Number = ' ',
        Boolean = ' ',
        Array = ' ',
        Object = ' ',
        Key = ' ',
        Null = ' ',
        EnumMember = ' ',
        Struct = ' ',
        Event = ' ',
        Operator = ' ',
        TypeParameter = ' '
      },
      lsp = {
        auto_attach = false,
        preference = nil,
      },
      highlight = true,
      separator = " > ",
      depth_limit = 0,
      depth_limit_indicator = "..",
      safe_output = true,
      click = false
    },
    event = "LspAttach",
    config = function(_, opts)
      require('nvim-navic').setup(opts)

      vim.api.nvim_create_autocmd("LspAttach", {
        group = vim.api.nvim_create_augroup("LspAttach_navic", {}),
        callback = function(args)
          if not (args.data and args.data.client_id) then
            return
          end

          local client = vim.lsp.get_client_by_id(args.data.client_id)
          require("nvim-navic").attach(client, args.buf)
        end
      })
    end,
  },

  {
    "folke/todo-comments.nvim",
    cmd = { "TodoTrouble", "TodoTelescope" },
    event = { "BufReadPost", "BufNewFile" },
    config = true,
    -- stylua: ignore
    keys = {
      { "]t",         function() require("todo-comments").jump_next() end, desc = "Next todo comment" },
      { "[t",         function() require("todo-comments").jump_prev() end, desc = "Previous todo comment" },
      { "<leader>xt", "<cmd>TodoTrouble<cr>",                              desc = "Todo (Trouble)" },
      { "<leader>xT", "<cmd>TodoTrouble keywords=TODO,FIX,FIXME<cr>",      desc = "Todo/Fix/Fixme (Trouble)" },
      { "<leader>st", "<cmd>TodoTelescope<cr>",                            desc = "Todo" },
      { "<leader>sT", "<cmd>TodoTelescope keywords=TODO,FIX,FIXME<cr>",    desc = "Todo/Fix/Fixme" },
    },
  },

  {
    "kevinhwang91/nvim-ufo",
    dependencies = "kevinhwang91/promise-async",
    event = "BufReadPost",
    opts = {},

    init = function()
      -- Using ufo provider need remap `zR` and `zM`. If Neovim is 0.6.1, remap yourself
      vim.keymap.set("n", "zR", function()
        require("ufo").openAllFolds()
      end)
      vim.keymap.set("n", "zM", function()
        require("ufo").closeAllFolds()
      end)
    end,
  },

  {
    "NvChad/nvim-colorizer.lua",
    event = "VeryLazy",
    opts = {
      filetypes = { "*", "!lazy" },
      buftype = { "*", "!prompt", "!nofile" },
      user_default_options = {
        RGB = true,       -- #RGB hex codes
        RRGGBB = true,    -- #RRGGBB hex codes
        names = false,    -- "Name" codes like Blue
        RRGGBBAA = true,  -- #RRGGBBAA hex codes
        AARRGGBB = false, -- 0xAARRGGBB hex codes
        rgb_fn = true,    -- CSS rgb() and rgba() functions
        hsl_fn = true,    -- CSS hsl() and hsla() functions
        css = false,      -- Enable all CSS features: rgb_fn, hsl_fn, names, RGB, RRGGBB
        css_fn = true,    -- Enable all CSS *functions*: rgb_fn, hsl_fn
        -- Available modes: foreground, background
        -- Available modes for `mode`: foreground, background,  virtualtext
        mode = "background", -- Set the display mode.
        virtualtext = "■",
      },
    },
  },

  {
    "smjonas/inc-rename.nvim",
    cmd = "IncRename",
    config = true,
  },

  {
    "Wansmer/treesj",

    use_default_keymaps = false,

    keys = {
      {
        '<leader>m',
        function()
          require('treesj').toggle()
        end,
      },
      {
        '<leader>M',
        function()
          require('treesj').toggle({
            split = {
              recursive = true,
            },
          })
        end,
      },
      {
        '<leader>j',
        function()
          require('treesj').join()
        end
      },
      {
        '<leader>s',
        function()
          require('treesj').split()
        end
      },
    },

    dependencies = { 'nvim-treesitter/nvim-treesitter' },

    config = true,
  },

  {
    "danymat/neogen",
    keys = {
      {
        "<leader>cc",
        function()
          require("neogen").generate({})
        end,
        desc = "Neogen Comment",
      },
    },
    opts = { snippet_engine = "luasnip" },
  },

  {
    "AckslD/muren.nvim",
    opts = {
      patterns_width = 60,
      patterns_height = 20,
      options_width = 40,
      preview_height = 24,
    },
    cmd = "MurenToggle",
  },

  {
    "sindrets/diffview.nvim",
    cmd = { "DiffviewOpen", "DiffviewClose", "DiffviewToggleFiles", "DiffviewFocusFiles" },
    config = function()
      local actions = require("diffview.actions")

      require('diffview').setup {
        view = {
          merge_tool = {
            layout = "diff3_mixed",
          }
        },

        keymaps = {
          diff3 = {
            { { "n", "x" }, "[g", actions.diffget("ours") },
            { { "n", "x" }, "]g", actions.diffget("theirs") },
          },
        },
      }
    end,
    keys = { { "<leader>gd", "<cmd>DiffviewOpen<cr>", desc = "DiffView" } },
  },

  {
    "ggandor/flit.nvim",
    keys = function()
      ---@type LazyKeys[]
      local ret = {}
      for _, key in ipairs({ "f", "F", "t", "T" }) do
        ret[#ret + 1] = { key, mode = { "n", "x", "o" }, desc = key }
      end
      return ret
    end,
    opts = { labeled_modes = "nx" },
  },

  {
    "ggandor/leap.nvim",
    keys = {
      { "s",  mode = { "n", "x", "o" }, desc = "Leap forward to" },
      { "S",  mode = { "n", "x", "o" }, desc = "Leap backward to" },
      { "gs", mode = { "n", "x", "o" }, desc = "Leap from windows" },
    },
    config = function(_, opts)
      local leap = require("leap")
      for k, v in pairs(opts) do
        leap.opts[k] = v
      end
      leap.add_default_mappings(true)
      vim.keymap.del({ "x", "o" }, "x")
      vim.keymap.del({ "x", "o" }, "X")
    end,
  },

  {
    'mhartington/formatter.nvim',

    keys = {
      { "<space>f", "<cmd>Format<cr>" }
    },

    opts = {
      filetype = {
        yaml = {
          function()
            return {
              exe = vim.fn.stdpath('data') .. '/mason/packages/yamlfmt/yamlfmt',
              args = { "-in" },
              stdin = true,
            }
          end
        },
      },
    },
  },

}
