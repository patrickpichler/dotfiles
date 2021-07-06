
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

Plug 'christianrondeau/vim-base64'
Plug 'neovim/nvim-lspconfig'
Plug 'kabouzeid/nvim-lspinstall'
Plug 'ray-x/lsp_signature.nvim'

Plug 'RishabhRD/popfix'
Plug 'RishabhRD/nvim-lsputils'

Plug 'liuchengxu/vista.vim'

Plug 'folke/trouble.nvim'

Plug 'hrsh7th/nvim-compe', { 'tag': '*' }

Plug 'nvim-treesitter/nvim-treesitter', {'do': ':TSUpdate'}  " We recommend updating the parsers on update

Plug 'nvim-lua/plenary.nvim'
Plug 'nvim-lua/popup.nvim'

Plug 'nvim-telescope/telescope.nvim'

Plug 'kyazdani42/nvim-tree.lua'

Plug 'lewis6991/gitsigns.nvim'

Plug 'arthurxavierx/vim-caser'

Plug 'editorconfig/editorconfig-vim'

Plug 'mattn/emmet-vim'
Plug 'skywind3000/asyncrun.vim'
Plug 'cohama/lexima.vim'

Plug 'junegunn/rainbow_parentheses.vim'

Plug 'itchyny/lightline.vim'
Plug 'easymotion/vim-easymotion'

Plug 'AndrewRadev/linediff.vim'
Plug 'AndrewRadev/inline_edit.vim'

Plug 'mbbill/undotree'

Plug 'SirVer/ultisnips'
Plug 'honza/vim-snippets'

Plug 'AndrewRadev/bufferize.vim'
Plug 'tommcdo/vim-exchange'
Plug 'junegunn/vim-peekaboo'

Plug 'machakann/vim-highlightedyank'

Plug 'leafgarland/typescript-vim', { 'for': 'typescript' }
Plug 'ziglang/zig.vim', { 'for': 'zig' }
Plug 'cespare/vim-toml', { 'for': 'toml' }
Plug 'udalov/kotlin-vim', { 'for': 'kotlin' }
Plug 'ekalinin/Dockerfile.vim', { 'for': 'dockerfile' }
Plug 'blankname/vim-fish', { 'for': 'fish' }
Plug 'neovimhaskell/haskell-vim', {'for': 'haskell'}
Plug 'ElmCast/elm-vim', {'for': 'elm'}
Plug 'mxw/vim-jsx', {'for': 'js'}
Plug 'rhysd/vim-crystal', { 'for': 'crystal' }

" ============= New text objects ==============
Plug 'kana/vim-textobj-user'
Plug 'vim-utils/vim-line'
Plug 'wellle/targets.vim'
Plug 'kana/vim-textobj-entire'

" ============= Clojure ==========================
Plug 'Olical/conjure', { 'for': ['clojure', 'fennel'], 'tag': '*' }

" ===========================================

Plug 'https://git.sr.ht/~patrickpichler/vim-github-colorscheme'
Plug 'https://tildegit.org/sloum/gemini-vim-syntax'

" ===========================================
call plug#end()

" }}}

" General configuration {{{
colorscheme github

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
set termguicolors

" code folding settings
set foldmethod=syntax " fold based on indent
set foldlevelstart=99
set foldnestmax=10 " deepest fold is 10 levels
set nofoldenable " don't fold by default
set foldlevel=1

set completeopt=noselect,menuone
set shortmess+=c

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


" Status Line {
set laststatus=2 " always show statusbar
set noshowmode

" }}}

" Lightline {{{
let g:lightline = {
      \ 'colorscheme': 'ayu_light',
      \ 'active': {
      \   'left': [ [ 'mode', 'paste', ],
      \             [ 'gitgutter', 'gitbranch', 'readonly', 'filename', 'modified' ] ]
      \ },
      \ 'component_function': {
      \   'gitbranch': 'fugitive#head',
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

autocmd User CocDiagnosticChange call lightline#update()

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


vim.lsp.handlers['textDocument/codeAction'] = require'lsputil.codeAction'.code_action_handler
vim.lsp.handlers['textDocument/references'] = require'lsputil.locations'.references_handler
vim.lsp.handlers['textDocument/definition'] = require'lsputil.locations'.definition_handler
vim.lsp.handlers['textDocument/declaration'] = require'lsputil.locations'.declaration_handler
vim.lsp.handlers['textDocument/typeDefinition'] = require'lsputil.locations'.typeDefinition_handler
vim.lsp.handlers['textDocument/implementation'] = require'lsputil.locations'.implementation_handler
vim.lsp.handlers['textDocument/documentSymbol'] = require'lsputil.symbols'.document_handler
vim.lsp.handlers['workspace/symbol'] = require'lsputil.symbols'.workspace_handler

local nvim_lsp = require('lspconfig')

-- Use an on_attach function to only map the following keys
-- after the language server attaches to the current buffer
local on_attach = function(client, bufnr)
  local function buf_set_keymap(...) vim.api.nvim_buf_set_keymap(bufnr, ...) end
  local function buf_set_option(...) vim.api.nvim_buf_set_option(bufnr, ...) end

  --Enable completion triggered by <c-x><c-o>
  -- buf_set_option('omnifunc', 'v:lua.vim.lsp.omnifunc')

  -- Mappings.
  local opts = { noremap=true, silent=true }

  -- See `:help vim.lsp.*` for documentation on any of the below functions
  buf_set_keymap('n', 'gD', '<Cmd>lua vim.lsp.buf.declaration()<CR>', opts)
  buf_set_keymap('n', 'gd', '<Cmd>lua vim.lsp.buf.definition()<CR>', opts)
  buf_set_keymap('n', 'K', '<Cmd>lua vim.lsp.buf.hover()<CR>', opts)
  buf_set_keymap('n', 'gi', '<cmd>lua vim.lsp.buf.implementation()<CR>', opts)
  buf_set_keymap('n', '<C-k>', '<cmd>lua vim.lsp.buf.signature_help()<CR>', opts)
  buf_set_keymap('n', '<space>wa', '<cmd>lua vim.lsp.buf.add_workspace_folder()<CR>', opts)
  buf_set_keymap('n', '<space>ws', ':Telescope lsp_dynamic_workspace_symbols<CR>', opts)
  buf_set_keymap('n', '<space>wd', ':Telescope lsp_workspace_diagnostics<CR>', opts)
  buf_set_keymap('n', '<space>s', ':Telescope lsp_document_symbols<CR>', opts)
  buf_set_keymap('n', '<space>wr', '<cmd>lua vim.lsp.buf.remove_workspace_folder()<CR>', opts)
  buf_set_keymap('n', '<space>wl', '<cmd>lua print(vim.inspect(vim.lsp.buf.list_workspace_folders()))<CR>', opts)
  buf_set_keymap('n', '<space>d', '<cmd>lua vim.lsp.buf.type_definition()<CR>', opts)
  buf_set_keymap('n', '<space>rn', '<cmd>lua vim.lsp.buf.rename()<CR>', opts)
  buf_set_keymap('n', '<space>a', '<cmd>lua vim.lsp.buf.code_action()<CR>', opts)
  buf_set_keymap('n', 'gr', '<cmd>lua vim.lsp.buf.references()<CR>', opts)
  buf_set_keymap('n', '<space>e', '<cmd>lua vim.lsp.diagnostic.show_line_diagnostics()<CR>', opts)
  buf_set_keymap('n', '[d', '<cmd>lua vim.lsp.diagnostic.goto_prev()<CR>', opts)
  buf_set_keymap('n', ']d', '<cmd>lua vim.lsp.diagnostic.goto_next()<CR>', opts)
  buf_set_keymap('n', '<space>q', '<cmd>lua vim.lsp.diagnostic.set_loclist()<CR>', opts)
  buf_set_keymap("n", "<space>f", "<cmd>lua vim.lsp.buf.formatting()<CR>", opts)

  require'lsp_signature'.on_attach()

  local cap = client.resolved_capabilities

  if cap.document_highlight then
    vim.cmd([[
      augroup LspHighlight
        autocmd!
        autocmd CursorHold   * lua vim.lsp.buf.document_highlight()
        autocmd CursorHoldI  * lua vim.lsp.buf.document_highlight()
        autocmd CursorMoved  * lua vim.lsp.buf.clear_references()
        autocmd CursorMovedI * lua vim.lsp.buf.clear_references()
      augroup END
    ]])
  end

end

vim.lsp.handlers["textDocument/publishDiagnostics"] = vim.lsp.with(
  vim.lsp.diagnostic.on_publish_diagnostics, {
    update_in_insert = true,
  }
)

-- Use a loop to conveniently call 'setup' on multiple servers and
-- map buffer local keybindings when the language server attaches
local servers = { "clangd", "rls", "clojure_lsp", "gopls", "zls",   }
for _, lsp in ipairs(servers) do
  nvim_lsp[lsp].setup {
    on_attach = on_attach,
    flags = {
      debounce_text_changes = 150,
    }
  }
end

nvim_lsp["jdtls"].setup {
  on_attach = on_attach,
  cmd = { 'jdtls.sh' },
  flags = {
    debounce_text_changes = 150,
  }
}

require'lspconfig'.jsonls.setup {
    on_attach = on_attach,
    flags = {
      debounce_text_changes = 150,
    },
    commands = {
      Format = {
        function()
          print("format")
          vim.lsp.buf.range_formatting({},{0,0},{vim.fn.line("$"),0})
        end
      }
    }
}

EOF

highlight LspReferenceText cterm=bold gui=bold ctermfg=green guifg=green
highlight LspReferenceRead cterm=bold gui=bold ctermfg=green guifg=green
highlight LspReferenceWrite cterm=bold gui=bold ctermfg=green guifg=green
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
highlight GitGutterAdd    ctermfg=green guifg=green
highlight GitGutterChange ctermfg=yellow guifg=yellow
highlight GitGutterDelete ctermfg=red guifg=red

highlight HighlightedyankRegion ctermbg=yellow guibg=yellow
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

let g:nvim_tree_auto_open = 1
let g:nvim_tree_lsp_diagnostics = 1
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

" vim-compe {{{
let g:lexima_no_default_rules = v:true
call lexima#set_default_rules()
inoremap <silent><expr> <C-Space> compe#complete()
inoremap <silent><expr> <CR>      compe#confirm(lexima#expand('<CR>', 'i'))
inoremap <silent><expr> <C-e>     compe#close('<C-e>')
inoremap <silent><expr> <C-f>     compe#scroll({ 'delta': +4 })
inoremap <silent><expr> <C-d>     compe#scroll({ 'delta': -4 })

let g:compe = {}
let g:compe.enabled = v:true
let g:compe.autocomplete = v:true
let g:compe.min_length = 1
let g:compe.preselect = 'disable'
let g:compe.throttle_time = 80
let g:compe.source_timeout = 200
let g:compe.resolve_timeout = 800
let g:compe.incomplete_delay = 400
let g:compe.max_abbr_width = 100
let g:compe.max_kind_width = 100
let g:compe.max_menu_width = 100
let g:compe.documentation = v:true

let g:compe.source = {}
let g:compe.source.path = v:true
let g:compe.source.buffer = v:true
let g:compe.source.calc = v:true
let g:compe.source.nvim_lsp = v:true
let g:compe.source.nvim_lua = v:true
let g:compe.source.ultisnips = v:true
let g:compe.source.emoji = v:true

" }}}

" Treesitter {{{
lua <<EOF
require'nvim-treesitter.configs'.setup {
  ensure_installed = "maintained", -- one of "all", "maintained" (parsers with maintainers), or a list of languages
  highlight = {
    enable = true,              -- false will disable the whole extension
  },
}
EOF

" }}}

" nvim-telescope {{{

nnoremap <M-p> <cmd>Telescope find_files<cr>
nnoremap <space>g <cmd>Telescope live_grep<cr>

" }}}

" UltiSnips {{{
let g:UltiSnipsExpandTrigger="<tab>"
let g:UltiSnipsJumpForwardTrigger="<c-j>"
let g:UltiSnipsJumpBackwardTrigger="<c-k>"

" If you want :UltiSnipsEdit to split your window.
let g:UltiSnipsEditSplit="vertical"

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

" vim: foldmethod=marker foldlevel=0 foldenable
