
" Plugins {{{

call plug#begin('~/.local/share/nvim/plugged')

" ======================================================

Plug 'tpope/vim-dispatch'
Plug 'tpope/vim-unimpaired'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-fugitive', { 'tag': '*' }
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-dotenv'
Plug 'tpope/vim-projectionist'

Plug 'RishabhRD/popfix'
Plug 'RishabhRD/nvim-lsputils'

Plug 'christianrondeau/vim-base64'
Plug 'neovim/nvim-lspconfig'
Plug 'williamboman/mason.nvim'
Plug 'williamboman/mason-lspconfig.nvim'

Plug 'RRethy/vim-illuminate'

Plug 'folke/lsp-colors.nvim'

Plug 'liuchengxu/vista.vim'

Plug 'folke/trouble.nvim'

Plug 'hrsh7th/cmp-nvim-lsp'
Plug 'hrsh7th/cmp-buffer'
Plug 'hrsh7th/cmp-path'
Plug 'hrsh7th/cmp-cmdline'
Plug 'hrsh7th/nvim-cmp'

Plug 'ray-x/cmp-treesitter'

Plug 'hrsh7th/cmp-vsnip'
Plug 'hrsh7th/vim-vsnip'
Plug 'hrsh7th/vim-vsnip-integ'
Plug 'rafamadriz/friendly-snippets'

Plug 'nvim-treesitter/nvim-treesitter', {'do': ':TSUpdate'}  " We recommend updating the parsers on update

Plug 'nvim-lua/plenary.nvim'
Plug 'nvim-lua/popup.nvim'

Plug 'nvim-telescope/telescope.nvim'
Plug 'nvim-telescope/telescope-ui-select.nvim'

Plug 'kyazdani42/nvim-tree.lua', { 'tag': '*' }

Plug 'lewis6991/gitsigns.nvim'

Plug 'arthurxavierx/vim-caser'

Plug 'editorconfig/editorconfig-vim'

Plug 'mattn/emmet-vim'
Plug 'cohama/lexima.vim'

Plug 'junegunn/rainbow_parentheses.vim'

Plug 'itchyny/lightline.vim'
Plug 'easymotion/vim-easymotion'

Plug 'AndrewRadev/linediff.vim'
Plug 'AndrewRadev/inline_edit.vim'

Plug 'mbbill/undotree'

Plug 'AndrewRadev/bufferize.vim'
Plug 'tommcdo/vim-exchange'
Plug 'junegunn/vim-peekaboo'

Plug 'machakann/vim-highlightedyank'
" ============= New text objects ==============
Plug 'kana/vim-textobj-user'
Plug 'vim-utils/vim-line'
Plug 'wellle/targets.vim'
Plug 'kana/vim-textobj-entire'

" ============= Clojure ==========================
Plug 'Olical/conjure', { 'for': ['clojure', 'fennel'], 'tag': '*' }

Plug 'Hoffs/omnisharp-extended-lsp.nvim'

" ===========================================

Plug 'https://tildegit.org/sloum/gemini-vim-syntax'
Plug 'NLKNguyen/papercolor-theme'

" ===========================================
call plug#end()

" }}}

" General configuration {{{

" Used Vim settings, rather than Vi settings (much better!).
" This must be first, because it changes other options as a side effect.
set nocompatible

" Make backspace behave in a sane manner.
"
" Make backspace behave in a sane manner.
set backspace=indent,eol,start

" Enable file type detection and do language-dependent indenting.
filetype on
filetype plugin indent on
filetype plugin on

" Show line numbers
set number

" Allow hidden buffers, don't limit to 1 file per window/split
set hidden

syntax on
syntax enable
syntax sync fromstart

" Enables search highlighting
set hlsearch

" Set indent to use spaces instead of tabs
set expandtab
set shiftwidth=2
set softtabstop=2
set tabstop=2

set smarttab
set autoindent
set smartindent

set splitbelow
set splitright

set inccommand=split

au BufNewFile,BufRead /dev/shm/gopass.* setlocal noswapfile nobackup noundofile

" Always use UTF-8
set termencoding=utf-8
set fileencoding=utf-8

set lazyredraw

if (has('termguicolors'))
  set termguicolors
endif

" code folding settings
set foldmethod=syntax " fold based on indent
set foldlevelstart=99
set foldnestmax=10 " deepest fold is 10 levels
set nofoldenable " don't fold by default
set foldlevel=1

set completeopt=menu,menuone,noselect
set shortmess+=c

" disable mouse support
set mouse=

" Backup and swap files {{{

" Protect changes between writes. Default values of
" updatecount (200 keystrokes) and updatetime
" (4 seconds) are fine
set swapfile
set directory^=~/.local/share/nvim/swap/

" protect against crash-during-write
set writebackup
" but do not persist backup after successful write
set nobackup
" use rename-and-write-new method whenever safe
set backupcopy=auto
" consolidate the writebackups -- not a big
" deal either way, since they usually get deleted
set backupdir^=~/.local/share/nvim/backup//

" persist the undo tree for each file
set undofile
" }}}

" Auto open quickfix list on grep
autocmd QuickFixCmdPost *grep* cwindow

" autohighlight word under cursor

set updatetime=10

" Map Ctrl+c to Esc, as Ctrl+C does not do some usefull things
map <C-c> <Esc>
map! <C-c> <Esc>


" Status Line {
set laststatus=2 " always show statusbar
set noshowmode

" }}}

" Lightline {{{
let g:lightline = {
      \ 'colorscheme': 'PaperColor',
      \ 'active': {
      \   'left': [ [ 'mode', 'paste', ],
      \             [ 'gitgutter', 'gitbranch', 'readonly', 'filename', 'modified' ] ]
      \ },
      \ 'component_function': {
      \   'gitbranch': 'FugitiveHead',
      \   'readonly': 'LightlineReadonly',
      \   'gitgutter': 'LightLineGitGutter',
      \   'filename': 'LightlineFileName',
      \ },
    \ }

function! LightlineFileName() abort
    let filename = winwidth(0) > 70 ? expand('%') : expand('%:t')
    let modified = &modified ? ' +' : ''
    return fnamemodify(filename, ":~:.") . modified
endfunction

function! LightlineReadonly()
  return &readonly && &filetype !=# 'help' ? 'RO' : ''
endfunction

function! LightLineGitGutter()
  if ! exists('*GitGutterGetHunkSummary')
        \ || ! get(g:, 'gitgutter_enabled', 0)
        \ || winwidth('.') <= 90
    return ''
  endif
  let symbols = ['+','~','-']
  let hunks = GitGutterGetHunkSummary()
  let ret = []
  for i in [0, 1, 2]
    if hunks[i] > 0
      call add(ret, symbols[i] . hunks[i])
    endif
  endfor
  return join(ret, ' ')
endfunction

" }}}

" remap leader key to something more reachable
let mapleader = ","

" LSP {{{

lua << EOF

local function has_value (tab, val)
    for index, value in ipairs(tab) do
        if value == val then
            return true
        end
    end

    return false
end

vim.lsp.handlers['textDocument/references'] = require'lsputil.locations'.references_handler
vim.lsp.handlers['textDocument/definition'] = require'lsputil.locations'.definition_handler
vim.lsp.handlers['textDocument/declaration'] = require'lsputil.locations'.declaration_handler
vim.lsp.handlers['textDocument/typeDefinition'] = require'lsputil.locations'.typeDefinition_handler
vim.lsp.handlers['textDocument/implementation'] = require'lsputil.locations'.implementation_handler
vim.lsp.handlers['textDocument/documentSymbol'] = require'lsputil.symbols'.document_handler
vim.lsp.handlers['workspace/symbol'] = require'lsputil.symbols'.workspace_handler

-- Use an on_attach function to only map the following keys
-- after the language server attaches to the current buffer
local on_attach = function(client, bufnr)
  local function buf_set_keymap(...) vim.api.nvim_buf_set_keymap(bufnr, ...) end
  local function buf_set_option(...) vim.api.nvim_buf_set_option(bufnr, ...) end

  -- Mappings.
  local opts = { noremap=true, silent=true }

  -- See `:help vim.lsp.*` for documentation on any of the below functions
  buf_set_keymap('n', 'gD', '<Cmd>lua vim.lsp.buf.declaration()<CR>', opts)
  buf_set_keymap('n', 'gd', '<Cmd>lua vim.lsp.buf.definition()<CR>', opts)
  buf_set_keymap('n', 'K', '<Cmd>lua vim.lsp.buf.hover()<CR>', opts)
  buf_set_keymap('n', 'gi', '<cmd>lua vim.lsp.buf.implementation()<CR>', opts)
  buf_set_keymap('n', '<C-k>', '<cmd>lua vim.lsp.buf.signature_help()<CR>', opts)
  buf_set_keymap('i', '<C-k>', '<cmd>lua vim.lsp.buf.signature_help()<CR>', opts)
  buf_set_keymap('n', '<space>wa', '<cmd>lua vim.lsp.buf.add_workspace_folder()<CR>', opts)
  buf_set_keymap('n', '<space>ws', ':Telescope lsp_dynamic_workspace_symbols<CR>', opts)
  buf_set_keymap('n', '<space>wd', ':Telescope lsp_workspace_diagnostics<CR>', opts)
  buf_set_keymap('n', '<space>s', ':Telescope lsp_document_symbols<CR>', opts)
  buf_set_keymap('n', '<space>wr', '<cmd>lua vim.lsp.buf.remove_workspace_folder()<CR>', opts)
  buf_set_keymap('n', '<space>wl', '<cmd>lua print(vim.inspect(vim.lsp.buf.list_workspace_folders()))<CR>', opts)
  buf_set_keymap('n', '<space>d', '<cmd>lua vim.lsp.buf.type_definition()<CR>', opts)
  buf_set_keymap('n', '<space>rn', '<cmd>lua vim.lsp.buf.rename()<CR>', opts)
  buf_set_keymap('n', '<space>a', '<cmd>lua vim.lsp.buf.code_action()<CR>', opts)
  buf_set_keymap('v', '<space>a', '<ESC><cmd>lua vim.lsp.buf.range_code_action()<CR>', opts)
  buf_set_keymap('n', 'gr', '<cmd>lua vim.lsp.buf.references()<CR>', opts)
  buf_set_keymap('n', '<space>e', '<cmd>lua vim.diagnostic.open_float()<CR>', opts)
  buf_set_keymap('n', '[d', '<cmd>lua vim.diagnostic.goto_prev()<CR>', opts)
  buf_set_keymap('n', ']d', '<cmd>lua vim.diagnostic.goto_next()<CR>', opts)
  buf_set_keymap('n', '<space>q', '<cmd>lua vim.lsp.diagnostic.set_loclist()<CR>', opts)
  buf_set_keymap('n', '<space>f', '<cmd>lua vim.lsp.buf.format { async = true }<CR>', opts)

  require 'illuminate'.on_attach(client)
end

vim.lsp.handlers["textDocument/publishDiagnostics"] = vim.lsp.with(
  vim.lsp.diagnostic.on_publish_diagnostics, {
    update_in_insert = true,
  }
)

local settings = {
  gopls = {
    gopls = {
      env = {GOFLAGS="-tags=unit"}
    }
  }
}

require("mason").setup()
require("mason-lspconfig").setup()
require("mason-lspconfig").setup_handlers {
  function (server_name) -- default handler (optional)
    local capabilities = vim.lsp.protocol.make_client_capabilities()
    capabilities.textDocument.completion.completionItem.snippetSupport = true
    capabilities.textDocument.completion.completionItem.resolveSupport = {
      properties = {
        'documentation',
        'detail',
        'additionalTextEdits',
      }
    }

    local opts = {
      capabilities = require('cmp_nvim_lsp').default_capabilities(capabilities),
      on_attach = on_attach,
      flags = {
        debounce_text_changes = 150,
      },
    }

    if server_name == 'efm' then
      opts.filetypes = {'python'}
    end

    if server_name == 'omnisharp' then
      opts.handlers = {
        ["textDocument/definition"] = require('omnisharp_extended').handler
      }
    end

    require("lspconfig")[server_name].setup(opts)
  end,

  ["jsonls"] = function()
    require'lspconfig'.jsonls.setup {
        on_attach = on_attach,
        flags = {
          debounce_text_changes = 150,
        },
        commands = {
          Format = {
            function()
              vim.lsp.buf.range_formatting({},{0,0},{vim.fn.line("$"),0})
            end
          }
        }
    }
  end
}

for _, lsp in ipairs({ "clangd", "rls", "clojure_lsp", "zls", "gopls" }) do
  local capabilities = vim.lsp.protocol.make_client_capabilities()
  capabilities.textDocument.completion.completionItem.snippetSupport = true
  capabilities.textDocument.completion.completionItem.resolveSupport = {
    properties = {
      'documentation',
      'detail',
      'additionalTextEdits',
    }
  }

  local config = {
    capabilities = require('cmp_nvim_lsp').default_capabilities(capabilities),
    on_attach = on_attach,
    flags = {
      debounce_text_changes = 150,
    },
  }

  if settings[lsp] ~= nil then
      config.settings = settings[lsp]
  end

  require('lspconfig')[lsp].setup(config)
end

EOF
" }}}

" Mappings {{{
"
" easier transition from terminal mode to normal mode
tnoremap <Esc> <C-\><C-n>

" send escape even the escape key is remapped
tnoremap <C-v><Esc> <Esc>

nnoremap <C-h> <C-w>h
nnoremap <C-j> <C-w>j
nnoremap <C-k> <C-w>k
nnoremap <C-l> <C-w>l

" Make C-P C-N behave the same as Up Down in command mode
cnoremap <C-P> <Up>
cnoremap <C-N> <Down>

" merge tool bindings
nnoremap <silent> [g :diffget //2<CR>
nnoremap <silent> ]g :diffget //3<CR>

" =========================================
nnoremap <silent><leader>ou :UndotreeToggle<CR>

noremap <M-Up> :res -2<CR>
noremap <M-Down> :res +2<CR>
noremap <M-Left> :vertical res -2<CR>
noremap <M-Right> :vertical res +2<CR>
" }}}

" Colorscheme {{{

let g:PaperColor_Theme_Options = {
  \   'theme': {
  \     'default': {
  \       'transparent_background': 1,
  \       'allow_bold': 1,
  \       'allow_italic': 1
  \     }
  \   }
  \ }

highlight HighlightedyankRegion ctermbg=yellow guibg=yellow

set background=light
colorscheme PaperColor

" }}}

" Filetypes {{{

augroup additional_ft
  au!

  autocmd BufNewFile,BufRead Jenkinsfile set ft=groovy
augroup END

" Transparent editing of gpg encrypted files.
" By Wouter Hanegraaff
augroup encrypted
  au!
  autocmd BufReadPre,FileReadPre *.gpg set viminfo=
  autocmd BufReadPre,FileReadPre *.gpg set noswapfile noundofile nobackup
  autocmd BufReadPre,FileReadPre *.gpg set bin
  autocmd BufReadPre,FileReadPre *.gpg let ch_save = &ch|set ch=2
  autocmd BufReadPost,FileReadPost *.gpg '[,']!gpg --decrypt 2> /dev/null
  autocmd BufReadPost,FileReadPost *.gpg set nobin
  autocmd BufReadPost,FileReadPost *.gpg let &ch = ch_save|unlet ch_save
  autocmd BufReadPost,FileReadPost *.gpg execute ":doautocmd BufReadPost " . expand("%:r")
  autocmd BufWritePre,FileWritePre *.gpg '[,']!gpg --default-recipient-self -ae 2>/dev/null
  autocmd BufWritePost,FileWritePost *.gpg u
augroup END

" }}}

" Markdown {{{
let g:markdown_fenced_languages = ['html', 'java', 'groovy', 'bash=sh',
      \ 'sh', 'kotlin']
" }}}

" UndoTree {{{

let g:undotree_SetFocusWhenToggle = 1
let g:undotree_ShortIndicators = 1
let g:undotree_WindowLayout = 2
" }}}

" Formating {{{
if executable('par')
  set formatprg=par\ -re

  autocmd FileType mail set formatprg=par\ -rjeq
endif

function! <SID>StripTrailingWhitespaces()
  if !&binary && &filetype != 'diff'
    let l:save = winsaveview()
    keeppatterns %s/\s\+$//e
    call winrestview(l:save)
  endif
endfun

autocmd BufWritePre,FileWritePre,FileAppendPre,FilterWritePre *
      \ :call <SID>StripTrailingWhitespaces()

" }}}

" nvim-tree {{{
lua <<EOF
require 'nvim-tree'.setup {
  view = {
    width = 40,
    adaptive_size = false,
  },
}
EOF

let g:nvim_tree_group_empty = 1
let g:nvim_tree_indent_markers = 1
let g:nvim_tree_show_icons = {
    \ 'git': 0,
    \ 'files': 0,
    \ 'folders': 1,
    \ 'folder_arrows': 0,
    \ }

let g:nvim_tree_icons = {
    \ 'folder': {
    \   'arrow_closed': ">",
    \   'arrow_open': "v",
    \   'default': ">",
    \   'open': "v",
    \   'empty': "-",
    \   'empty_open': "-",
    \   'symlink': "~",
    \   'symlink_open': "V",
    \   }
    \ }

nnoremap <leader>k :NvimTreeToggle<CR>
nnoremap <leader>r :NvimTreeRefresh<CR>
nnoremap - :NvimTreeFindFile<CR>

" }}}

" rainbow_parentheses {{{

let g:rainbow#pairs = [['(', ')'], ['[', ']'], ['{', '}']]
let g:rainbow#blacklist = ['#ffffff']

augroup rainbow
  autocmd!
  autocmd FileType * RainbowParentheses
augroup END

" }}}

" Clojure {{{
let g:conjure#mapping#doc_word = 'nil'
let g:conjure#mapping#def_word = 'nil'

autocmd BufNewFile,BufRead *.boot set filetype=clojure
" }}}

" vim-commentary {{{
autocmd FileType asm setlocal commentstring=;\ %s
"}}}

" Gitsigns {{{
highlight SignColumn guibg=#ECECEC

lua << EOF
require('gitsigns').setup {
  update_debounce = 500,
}
EOF
" }}}

" nvim-cmp {{{
lua <<EOF
  -- Setup nvim-cmp.
  local cmp = require'cmp'

  cmp.setup({
    snippet = {
      expand = function(args)
        vim.fn["vsnip#anonymous"](args.body)
      end,
    },
    mapping = {
      ['<C-d>'] = cmp.mapping.scroll_docs(-4),
      ['<C-f>'] = cmp.mapping.scroll_docs(4),
      ['<C-Space>'] = cmp.mapping.complete(),
      ['<C-e>'] = cmp.mapping.close(),
      ['<CR>'] = cmp.mapping.confirm({ select = true }),
      ['<C-p>'] = cmp.mapping.select_prev_item(),
      ['<C-n>'] = cmp.mapping.select_next_item(),
    },
    sources = {
      { name = 'nvim_lsp' },
      { name = 'vsnip' },
      { name = 'buffer' },
      { name = 'path' },
      { name = 'treesitter' },
    }
  })

  -- Set configuration for specific filetype.
  cmp.setup.filetype('gitcommit', {
    sources = cmp.config.sources({
      { name = 'cmp_git' }, -- You can specify the `cmp_git` source if you were installed it.
    }, {
      { name = 'buffer' },
    })
  })

  -- Use buffer source for `/` and `?` (if you enabled `native_menu`, this won't work anymore).
  cmp.setup.cmdline({ '/', '?' }, {
    mapping = cmp.mapping.preset.cmdline(),
    sources = {
      { name = 'buffer' }
    }
  })

  -- Use cmdline & path source for ':' (if you enabled `native_menu`, this won't work anymore).
  cmp.setup.cmdline(':', {
    mapping = cmp.mapping.preset.cmdline(),
    sources = cmp.config.sources({
      { name = 'path' }
    }, {
      { name = 'cmdline' }
    })
  })
EOF

" }}}

" Treesitter {{{
lua <<EOF
require'nvim-treesitter.configs'.setup {
  ensure_installed = "all",
  ignore_install = { "phpdoc" },
  highlight = { enable = true },
  indent = { enable = true },
  autotag = { enable = true },
}
EOF

" }}}

" nvim-telescope {{{

lua <<EOF
  require'telescope'.setup {
    defaults = { vimgrep_arguments = { 'rg', '--hidden', '--color=never', '--no-heading', '--with-filename', '--line-number', '--column', '--smart-case', '-g', '!.git' } },
  }

  require('telescope').load_extension('ui-select')
EOF

nnoremap <M-p> <cmd>lua require'telescope.builtin'.find_files({ find_command = {'rg', '--files', '--hidden', '-g', '!.git' }})<cr>
nnoremap <space>g <cmd>Telescope live_grep<cr>

" }}}

" vim-vsnip {{{
" Expand
imap <expr> <C-j>   vsnip#expandable()  ? '<Plug>(vsnip-expand)'         : '<C-j>'
smap <expr> <C-j>   vsnip#expandable()  ? '<Plug>(vsnip-expand)'         : '<C-j>'

" Expand or jump
imap <expr> <C-l>   vsnip#available(1)  ? '<Plug>(vsnip-expand-or-jump)' : '<C-l>'
smap <expr> <C-l>   vsnip#available(1)  ? '<Plug>(vsnip-expand-or-jump)' : '<C-l>'

" Jump forward or backward
imap <expr> <Tab>   vsnip#jumpable(1)   ? '<Plug>(vsnip-jump-next)'      : '<Tab>'
smap <expr> <Tab>   vsnip#jumpable(1)   ? '<Plug>(vsnip-jump-next)'      : '<Tab>'
imap <expr> <S-Tab> vsnip#jumpable(-1)  ? '<Plug>(vsnip-jump-prev)'      : '<S-Tab>'
smap <expr> <S-Tab> vsnip#jumpable(-1)  ? '<Plug>(vsnip-jump-prev)'      : '<S-Tab>'

" Select or cut text to use as $TM_SELECTED_TEXT in the next snippet.
" See https://github.com/hrsh7th/vim-vsnip/pull/50
nmap        s   <Plug>(vsnip-select-text)
xmap        s   <Plug>(vsnip-select-text)
nmap        S   <Plug>(vsnip-cut-text)
xmap        S   <Plug>(vsnip-cut-text)
" }}}

" vista {{{

let g:vista#renderer#enable_icon = 0

" }}}

" trouble {{{

lua << EOF
require("trouble").setup {
  icons = false,
  fold_open = "v", -- icon used for open folds
  fold_closed = ">", -- icon used for closed folds
  indent_lines = false, -- add an indent guide below the fold icons
  signs = {
      -- icons / text used for a diagnostic
      error = "error",
      warning = "warn",
      hint = "hint",
      information = "info"
  },
  use_lsp_diagnostic_signs = false -- enabling this will use the signs defined in your lsp clien
}
EOF

" }}}

" vim-illumiinate {{{
let g:Illuminate_ftblacklist = ['NvimTree', 'fugitiveblame']
" }}}

" vim: foldmethod=marker foldlevel=0 foldenable
