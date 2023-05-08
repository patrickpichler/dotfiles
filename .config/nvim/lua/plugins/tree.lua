return {
  {
    "nvim-neo-tree/neo-tree.nvim",
    version = "v2.*",

    dependencies = {
      "nvim-lua/plenary.nvim",
      "nvim-tree/nvim-web-devicons", -- not strictly required, but recommended
      "MunifTanjim/nui.nvim",
      {
        's1n7ax/nvim-window-picker',
        version = 'v1.*',
        config = function()
          require 'window-picker'.setup()
        end,

      },
    },
    opts = {
      filesystem = {
        use_libuv_file_watcher = true,
        group_empty_dirs = true,
      },

      event_handlers = {
        {
          event = "neo_tree_window_after_open",
          handler = function(args)
            if args.position == "left" or args.position == "right" then
              vim.cmd("wincmd =")
            end
          end
        },
        {
          event = "neo_tree_window_after_close",
          handler = function(args)
            if args.position == "left" or args.position == "right" then
              vim.cmd("wincmd =")
            end
          end
        }
      },

      window = {
        mappings = {
          ["h"] = function(state)
            local node = state.tree:get_node()
            if node.type == 'directory' and node:is_expanded() then
              require 'neo-tree.sources.filesystem'.toggle_directory(state, node)
            else
              require 'neo-tree.ui.renderer'.focus_node(state, node:get_parent_id())
            end
          end,
          ["l"] = function(state)
            local node = state.tree:get_node()
            if node.type == 'directory' then
              if not node:is_expanded() then
                require 'neo-tree.sources.filesystem'.toggle_directory(state, node)
              elseif node:has_children() then
                require 'neo-tree.ui.renderer'.focus_node(state, node:get_child_ids()[1])
              end
            end
          end,
        },
      },

    },

    config = function(_, opts)
      require('neo-tree').setup(opts)

      local kOpts = { noremap = true, silent = true }

      vim.keymap.set('n', '<leader>k', ':Neotree toggle<CR>', kOpts)
      vim.keymap.set('n', '-', ':Neotree focus reveal<CR>', kOpts)
    end,
  }
}
