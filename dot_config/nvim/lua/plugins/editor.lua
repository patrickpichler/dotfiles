return {
  { "echasnovski/mini.ai",     version = "*", config = true },
  { "echasnovski/mini.align",  version = "*", config = true },
  { "echasnovski/mini.basics", version = "*", config = true },
  { "echasnovski/mini.pairs",  version = "*", config = true },
  {
    "echasnovski/mini.surround",
    version = "*",
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
    "echasnovski/mini.bracketed",
    version = "*",
    opts = {
      yank = { suffix = "" },
      treesitter = { suffix = "n" },
    }
  },

  {
    "numToStr/Comment.nvim",
    opts = {
    },
    lazy = false,
  },

  { "cappyzawa/trim.nvim", config = true },
  {
    "RRethy/vim-illuminate",

    event = { "VeryLazy" },

    opts = {
      filetypes_denylist = {
        "neo-tree",
        "fugitive",
        "mason",
        "Trouble",
        "notify",
        "help",
        "Outline",
        "TelescopePrompt",
      },
      under_cursor = false,

      large_file_cutoff = 500,
    },

    config = function(_, opts)
      require("illuminate").configure(opts)
    end,
  },

  { "vim-utils/vim-line", },
  {
    "kana/vim-textobj-entire",
    dependencies = {
      { "kana/vim-textobj-user", },
    },
  },

  {
    "lukas-reineke/indent-blankline.nvim",
    event = { "VeryLazy" },
    main = "ibl",
    opts = {}
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
    "Wansmer/treesj",

    use_default_keymaps = false,

    keys = {
      {
        "<leader>m",
        function()
          require("treesj").toggle()
        end,
      },
      {
        "<leader>M",
        function()
          require("treesj").toggle({
            split = {
              recursive = true,
            },
          })
        end,
      },
      {
        "<leader>tj",
        function()
          require("treesj").join()
        end
      },
      {
        "<leader>ts",
        function()
          require("treesj").split()
        end
      },
    },

    dependencies = { "nvim-treesitter/nvim-treesitter" },

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

      require("diffview").setup {
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
    'stevearc/conform.nvim',
    dependencies = { "williamboman/mason.nvim" },
    keys = {
      {
        "<space>f",
        function()
          require("conform").format({ lsp_fallback = true })
        end,
        mode = { "n", "v" },
        desc = "Format",
      },
    },
    opts = function()
      local mason_reg = require "mason-registry"

      local formatters = {}
      local formatters_by_ft = {}

      for _, pkg in pairs(mason_reg.get_installed_packages()) do
        for _, type in pairs(pkg.spec.categories) do
          -- only act upon a formatter
          if type == "Formatter" then
            -- if formatter doesn't have a builtin config, create our own from a generic template
            if not require "conform".get_formatter_config(pkg.spec.name) then
              -- the key of the entry to this table
              -- is the name of the bare executable
              -- the actual value may not be the absolute path
              -- in some cases
              local bin = next(pkg.spec.bin)
              -- this should be replaced by a function
              -- that quieries the configured mason install path
              local prefix = vim.fn.stdpath("data") .. "/mason/bin/"

              formatters[pkg.spec.name] = {
                command = prefix .. bin,
                args = { "$FILENAME" },
                stdin = true,
                require_cwd = false
              }
            end

            -- finally add the formatter to it's compatible filetype(s)
            for _, ft in pairs(pkg.spec.languages) do
              local ftl = string.lower(ft)
              formatters_by_ft[ftl] = formatters_by_ft[ftl] or {}
              table.insert(formatters_by_ft[ftl], pkg.spec.name)

              if ftl == 'protobuf' then -- this hack is needed, as treesitter detects protobuf as proto ft
                formatters_by_ft['proto'] = formatters_by_ft['proto'] or {}
                table.insert(formatters_by_ft['proto'], pkg.spec.name)
              end
            end
          end
        end
      end

      return {
        lsp_fallback = true,
        formatters = formatters,
        formatters_by_ft = formatters_by_ft
      }
    end,
  },

  {
    "folke/todo-comments.nvim",
    cmd = { "TodoTrouble", "TodoTelescope" },
    event = { "BufReadPost", "BufNewFile" },
    opts = {
      highlight = {
        keyword = "bg",
        after = "fg",
        pattern = [[.*((KEYWORDS)(\([^)]*\)|)):]]
      },
      search = {
        command = "rg",
        args = {
          "--color=never",
          "--no-heading",
          "--with-filename",
          "--line-number",
          "--column",
        },
        -- regex that will be used to match keywords.
        -- don"t replace the (KEYWORDS) placeholder
        pattern = [[\b(KEYWORDS)\([^)]*\)?:]], -- ripgrep regex
        -- pattern = [[\b(KEYWORDS)\b]], -- match without the extra colon. You"ll likely get false positives
      },
    },
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
          enabled = false,
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
        marks = true,       -- shows a list of your marks on " and `
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
      ignore_missing = false,                                                       -- enable this to hide mappings for which you didn"t specify a label
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
    "mbbill/undotree",
    keys = {
      { "<leader>tu", vim.cmd.UndotreeToggle, desc = "Toggle UndoTree" },
    },
  },

  {
    "stevearc/dressing.nvim",
    opts = {},
  },

  {
    "gbprod/yanky.nvim",
    opts = {
      ring = { history_length = 20 },
      highlight = { timer = 250 },
    },
    keys = {
      { "p",  "<Plug>(YankyPutAfter)",          mode = { "n", "x" },                         desc = "Put yanked text after cursor" },
      { "P",  "<Plug>(YankyPutBefore)",         mode = { "n", "x" },                         desc = "Put yanked text before cursor" },
      { "=p", "<Plug>(YankyPutAfterLinewise)",  desc = "Put yanked text in line below" },
      { "=P", "<Plug>(YankyPutBeforeLinewise)", desc = "Put yanked text in line above" },
      { "[y", "<Plug>(YankyCycleForward)",      desc = "Cycle forward through yank history" },
      { "]y", "<Plug>(YankyCycleBackward)",     desc = "Cycle backward through yank history" },
    },
  },

  {
    "patrickpichler/hovercraft.nvim",

    event = "VeryLazy",

    keys = {
      { "K", function()
        local hovercraft = require("hovercraft")

        if hovercraft.is_visible() then
          hovercraft.enter_popup()
        else
          hovercraft.hover()
        end
      end },
    },
  },

  {
    "ThePrimeagen/harpoon",

    branch = "harpoon2",

    dependencies = {
      { "nvim-lua/plenary.nvim" },
      { "nvim-telescope/telescope.nvim" },
    },

    opts = {},

    init = function()
      local harpoon = require("harpoon")

      vim.keymap.set("n", "<leader>hw", function() harpoon.ui:toggle_quick_menu(harpoon:list()) end,
        { desc = "Open [h]arpoon [w]indow" })

      vim.keymap.set("n", "<leader>ha", function() harpoon:list():add() end,
        { desc = "[h]arpoon [a]dd" })
      vim.keymap.set("n", "<leader>1", function() harpoon:list():select(1) end)
      vim.keymap.set("n", "<leader>2", function() harpoon:list():select(2) end)
      vim.keymap.set("n", "<leader>3", function() harpoon:list():select(3) end)
      vim.keymap.set("n", "<leader>4", function() harpoon:list():select(4) end)

      vim.keymap.set("n", "<leader>hp", function() harpoon:list():prev() end,
        { desc = "[h]arpoon [p]rev" })
      vim.keymap.set("n", "<leader>hn", function() harpoon:list():next() end,
        { desc = "[h]arpoon [n]ext" })
    end,
  },

  {
    "ThePrimeagen/refactoring.nvim",

    dependencies = {
      { "nvim-telescope/telescope.nvim" },
      { "nvim-lua/plenary.nvim" },
    },

    opts = {},

    init = function()
      vim.keymap.set("x", "<leader>re", function() require("refactoring").refactor("Extract Function") end,
        { desc = "[R]efactoring [e]xtract function" })
      vim.keymap.set("x", "<leader>rf", function() require("refactoring").refactor("Extract Function To File") end,
        { desc = "[R]efactoring extract function to [f]ile" })
      -- Extract function supports only visual mode
      vim.keymap.set("x", "<leader>rv", function() require("refactoring").refactor("Extract Variable") end,
        { desc = "[R]efactoring extract [v]ariable" })
      -- Extract variable supports only visual mode
      vim.keymap.set("n", "<leader>rI", function() require("refactoring").refactor("Inline Function") end,
        { desc = "[R]efactoring [I]nline function" })
      -- Inline func supports only normal
      vim.keymap.set({ "n", "x" }, "<leader>ri", function() require("refactoring").refactor("Inline Variable") end,
        { desc = "[R]efactoring [i]nline variable" })
      -- Inline var supports both normal and visual mode

      vim.keymap.set("n", "<leader>rb", function() require("refactoring").refactor("Extract Block") end,
        { desc = "[R]efactoring extract [b]lock" })
      vim.keymap.set("n", "<leader>rbf", function() require("refactoring").refactor("Extract Block To File") end,
        { desc = "[R]efactoring extract [b]lock to [f]ile" })

      -- Extract block supports only normal mode
      -- load refactoring Telescope extension
      require("telescope").load_extension("refactoring")

      vim.keymap.set(
        { "n", "x" },
        "<leader>rr",
        function() require("telescope").extensions.refactoring.refactors() end,
        { desc = "[R]efactoring telescope" }
      )
    end,
  }
}
