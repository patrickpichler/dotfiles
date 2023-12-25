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
    "lukas-reineke/indent-blankline.nvim",
    event = { 'VeryLazy' },
    main = "ibl",
    opts = {}
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
      end, { desc = "Aerial toggle" })
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
    "kevinhwang91/nvim-ufo",
    dependencies = "kevinhwang91/promise-async",
    event = "BufReadPost",
    opts = {},

    init = function()
      -- Using ufo provider need remap `zR` and `zM`. If Neovim is 0.6.1, remap yourself
      vim.keymap.set("n", "zR", function()
        require("ufo").openAllFolds()
      end, { desc = "Open all folds" })
      vim.keymap.set("n", "zM", function()
        require("ufo").closeAllFolds()
      end, { desc = "Close all folds" })
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
    event = { 'VeryLazy' },
    config = function()
      require("inc_rename").setup()
    end,
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
    keys = { { "<leader>gd", vim.cmd.DiffviewOpen, desc = "DiffView" } },
  },

  {
    'mhartington/formatter.nvim',

    keys = {
      { "<space>f", vim.cmd.Format, desc = "Format" }
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

  {
    "folke/todo-comments.nvim",
    cmd = { "TodoTrouble", "TodoTelescope" },
    event = { "BufReadPost", "BufNewFile" },
    config = true,
    -- stylua: ignore
    keys = {
      { "]t",         function() require("todo-comments").jump_next() end,             desc = "Next todo comment" },
      { "[t",         function() require("todo-comments").jump_prev() end,             desc = "Previous todo comment" },
      { "<leader>xt", vim.cmd.TodoTrouble,                                             desc = "Todo (Trouble)" },
      { "<leader>xT", function() vim.cmd.TodoTrouble("keywords=TODO,FIX,FIXME") end,   desc = "Todo/Fix/Fixme (Trouble)" },
      { "<leader>st", vim.cmd.TodoTelescope,                                           desc = "[S]earch [t]odo" },
      { "<leader>sT", function() vim.cmd.TodoTelescope("keywords=TODO,FIX,FIXME") end, desc = "[S] [T]odo/Fix/Fixme" },
    },
  },

  {
    "folke/flash.nvim",
    event = "VeryLazy",
    opts = {
      modes = {
        char = {
          autohide = true,
        },

        search = {
          incremental = true,
        },
      },
    },
    -- stylua: ignore
    keys = {
      { "s",     mode = { "n", "x", "o" }, function() require("flash").jump() end,              desc = "Flash" },
      { "S",     mode = { "n", "x", "o" }, function() require("flash").treesitter() end,        desc = "Flash Treesitter" },
      { "r",     mode = "o",               function() require("flash").remote() end,            desc = "Remote Flash" },
      { "R",     mode = { "o", "x" },      function() require("flash").treesitter_search() end, desc = "Treesitter Search" },
      { "<c-s>", mode = { "c" },           function() require("flash").toggle() end,            desc = "Toggle Flash Search" },
    },
  },

  {
    "folke/which-key.nvim",
    event = "VeryLazy",
    init = function()
      vim.o.timeout = true
      vim.o.timeoutlen = 300
    end,
    opts = {
      plugins = {
        marks = true,       -- shows a list of your marks on ' and `
        registers = true,   -- shows your registers on " in NORMAL or <C-r> in INSERT mode
        spelling = {
          enabled = true,   -- enabling this will show WhichKey when pressing z= to select spelling suggestions
          suggestions = 20, -- how many suggestions should be shown in the list?
        },
        -- the presets plugin, adds help for a bunch of default keybindings in Neovim
        -- No actual key bindings are created
        presets = {
          operators = false,    -- adds help for operators like d, y, ... and registers them for motion / text object completion
          motions = false,      -- adds help for motions
          text_objects = false, -- help for text objects triggered after entering an operator
          windows = true,       -- default bindings on <c-w>
          nav = true,           -- misc bindings to work with windows
          z = true,             -- bindings for folds, spelling and others prefixed with z
          g = true,             -- bindings for prefixed with g
        },
      },
      -- add operators that will trigger motion and text object completion
      -- to enable all native operators, set the preset / operators plugin above
      operators = { gc = "Comments" },
      ignore_missing = false,                                                       -- enable this to hide mappings for which you didn't specify a label
      hidden = { "<silent>", "<cmd>", "<Cmd>", "<CR>", "call", "lua", "^:", "^ " }, -- hide mapping boilerplate
      show_help = true,                                                             -- show help message on the command line when the popup is visible
      triggers = "auto",                                                            -- automatically setup triggers
      -- triggers = {"<leader>"} -- or specify a list manually
      triggers_blacklist = {
        -- list of mode / prefixes that should never be hooked by WhichKey
        -- this is mostly relevant for key maps that start with a native binding
        -- most people should not need to change this
        i = { "j", "k" },
        v = { "j", "k" },
      },
      -- disable the WhichKey popup for certain buf types and file types.
      -- Disabled by deafult for Telescope
      disable = {
        buftypes = {},
        filetypes = { "TelescopePrompt" },
      },
    },
  },

  {
    'mbbill/undotree',
    keys = {
      { "<leader>tu", vim.cmd.UndotreeToggle, desc = "Toggle UndoTree" },
    },
  },

  {
    'gbprod/yanky.nvim',
    opts = {
      ring = { history_length = 20 },
      highlight = { timer = 250 },
    },
    keys = {
      { 'p',  '<Plug>(YankyPutAfter)',          mode = { 'n', 'x' },                         desc = 'Put yanked text after cursor' },
      { 'P',  '<Plug>(YankyPutBefore)',         mode = { 'n', 'x' },                         desc = 'Put yanked text before cursor' },
      { '=p', '<Plug>(YankyPutAfterLinewise)',  desc = 'Put yanked text in line below' },
      { '=P', '<Plug>(YankyPutBeforeLinewise)', desc = 'Put yanked text in line above' },
      { '[y', '<Plug>(YankyCycleForward)',      desc = 'Cycle forward through yank history' },
      { ']y', '<Plug>(YankyCycleBackward)',     desc = 'Cycle backward through yank history' },
    },

  }
}
