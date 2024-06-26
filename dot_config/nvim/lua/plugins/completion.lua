return {
  {
    "hrsh7th/nvim-cmp",

    event = { "VeryLazy" },

    dependencies = {
      { "hrsh7th/cmp-buffer" },
      { "hrsh7th/cmp-cmdline" },
      { "hrsh7th/cmp-nvim-lsp" },
      { "hrsh7th/cmp-path" },
      { "hrsh7th/cmp-emoji" },
      { "ray-x/cmp-treesitter" },
      { "saadparwaiz1/cmp_luasnip" },
      { "L3MON4D3/LuaSnip" },
    },

    config = function()
      local cmp = require "cmp"
      local luasnip = require("luasnip")

      local has_words_before = function()
        unpack = unpack or table.unpack
        local line, col = unpack(vim.api.nvim_win_get_cursor(0))
        return col ~= 0 and vim.api.nvim_buf_get_lines(0, line - 1, line, true)[1]:sub(col, col):match("%s") == nil
      end

      ---@diagnostic disable-next-line: missing-fields
      cmp.setup({

        snippet = {
          expand = function(args)
            require("luasnip").lsp_expand(args.body)
          end,
        },

        window = {
          completion = cmp.config.window.bordered(),
          documentation = cmp.config.window.bordered()
        },

        mapping = cmp.mapping.preset.insert {
          ["<C-u>"] = cmp.mapping.scroll_docs(-4),
          ["<C-d>"] = cmp.mapping.scroll_docs(4),
          ["<C-y>"] = cmp.mapping({
            i = function()
              cmp.confirm({ behavior = cmp.ConfirmBehavior.Replace, select = false })
            end,
            s = cmp.mapping.confirm({ select = true }),
          }),
          ["<C-p>"] = cmp.mapping.select_prev_item({ behavior = cmp.SelectBehavior.Select }),
          ["<C-n>"] = cmp.mapping.select_next_item({ behavior = cmp.SelectBehavior.Select }),
          ["<Up>"] = cmp.mapping.select_prev_item({ behavior = cmp.SelectBehavior.Select }),
          ["<Down>"] = cmp.mapping.select_next_item({ behavior = cmp.SelectBehavior.Select }),
          ["<Tab>"] = cmp.mapping(function(fallback)
            -- You could replace the expand_or_jumpable() calls with expand_or_locally_jumpable()
            -- they way you will only jump inside the snippet region
            if luasnip.locally_jumpable() then
              luasnip.expand_or_jump()
            elseif cmp.visible() then
              cmp.select_next_item({ behavior = cmp.SelectBehavior.Select })
            elseif has_words_before() then
              cmp.complete()
            else
              fallback()
            end
          end, { "i", "s" }),

          ["<S-Tab>"] = cmp.mapping(function(fallback)
            if luasnip.locally_jumpable(-1) then
              luasnip.jump(-1)
            elseif cmp.visible() then
              cmp.select_prev_item({ behavior = cmp.SelectBehavior.Select })
            else
              fallback()
            end
          end, { "i", "s" }),
        },

        sources = cmp.config.sources {
          { name = "nvim_lsp" },
          { name = "nvim_lsp_signature_help" },
          { name = "luasnip" },
          { name = "buffer" },
          { name = "path" },
          { name = "treesitter" },
          { name = "emoji" },
        }
      })

      -- Use buffer source for `/` and `?` (if you enabled `native_menu`, this won"t work anymore).
      cmp.setup.cmdline({ "/", "?" }, {
        mapping = cmp.mapping.preset.cmdline(),
        sources = {
          { name = "buffer" }
        }
      })

      -- Use cmdline & path source for ":" (if you enabled `native_menu`, this won"t work anymore).
      cmp.setup.cmdline(":", {
        mapping = cmp.mapping.preset.cmdline({
          ["<Tab>"] = {
            c = function(fallback)
              if cmp.visible() then
                cmp.select_next_item()
              else
                vim.api.nvim_feedkeys(vim.api.nvim_replace_termcodes("<C-z>", true, true, true), "ni", true)
              end
            end
          }
        }),
        sources = cmp.config.sources({
          { name = "path" }
        }, {
          { name = "cmdline" }
        })
      })
    end
  },

  {
    "L3MON4D3/LuaSnip",
    -- follow latest release.
    version = "1.*",
    -- install jsregexp (optional!).
    build = "make install_jsregexp",

    dependencies = {
      { "rafamadriz/friendly-snippets" }
    },

    config = function()
      local luasnip = require("luasnip")

      luasnip.setup({
        enable_autosnippets = true
      })

      require("luasnip.loaders.from_lua").load({ paths = "./snippets" })

      require("luasnip.loaders.from_vscode").lazy_load()

      vim.cmd [[
        imap <silent><expr> <C-E> luasnip#choice_active() ? "<Plug>luasnip-next-choice" : "<C-E>"
        smap <silent><expr> <C-E> luasnip#choice_active() ? "<Plug>luasnip-next-choice" : "<C-E>"
        imap <silent><expr> <C-S-E> luasnip#choice_active() ? "<Plug>luasnip-prev-choice" : "<C-S-E>"
        smap <silent><expr> <C-S-E> luasnip#choice_active() ? "<Plug>luasnip-prev-choice" : "<C-S-E>"
      ]]

      vim.api.nvim_create_autocmd("ModeChanged", {
        pattern = "*",
        callback = function()
          if ((vim.v.event.old_mode == "s" and vim.v.event.new_mode == "n") or vim.v.event.old_mode == "i")
              and require("luasnip").session.current_nodes[vim.api.nvim_get_current_buf()]
              and not require("luasnip").session.jump_active
          then
            require("luasnip").unlink_current()
          end
        end
      })
    end,
  }
}
