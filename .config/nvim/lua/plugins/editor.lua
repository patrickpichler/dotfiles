return {
  { 'tpope/vim-unimpaired' },
  { 'tpope/vim-repeat' },
  { 'cappyzawa/trim.nvim', opts = true },
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

  { 'machakann/vim-sandwich', },
  { 'mattn/emmet-vim', },
  { 'arthurxavierx/vim-caser', },
  { 'ggandor/lightspeed.nvim', },
  { 'vim-utils/vim-line', },
  {
    'kana/vim-textobj-entire',
    dependencies = {
      { 'kana/vim-textobj-user', },
    },
  },

  {
    'numToStr/Comment.nvim',

    event = { 'VeryLazy' },

    opts = true,
  },

  {
    'windwp/nvim-autopairs',

    event = { 'VeryLazy' },

    dependencies = {
      { 'hrsh7th/nvim-cmp' }
    },

    config = function()
      require("nvim-autopairs").setup {}

      local cmp_autopairs = require('nvim-autopairs.completion.cmp')
      local cmp = require('cmp')
      cmp.event:on(
        'confirm_done',
        cmp_autopairs.on_confirm_done()
      )
    end
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
    'folke/todo-comments.nvim',
    version = '*',
    opts = true,
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

}
