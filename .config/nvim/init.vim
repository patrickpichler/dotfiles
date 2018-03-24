" Used Vim settings, rather than Vi settings (much better!).
" This must be first, because it changes other options as a side effect.
set nocompatible

" Make backspace behave in a sane manner.
" Make backspace behave in a sane manner.
set backspace=indent,eol,start

" Enable file type detection and do language-dependent indenting.
filetype plugin indent on

" Show line numbers
set number

" Allow hidden buffers, don't limit to 1 file per window/split
set hidden


syntax on
syntax enable

" Enables search highlichting
set hlsearch

" Set indent to use spaces instead of tabs
set expandtab
set shiftwidth=2
set softtabstop=2

set smarttab 
set autoindent
set smartindent

" Detect .md as markdown instead of modula-l2
autocmd BufNewFile,BufReadPost *.md set filetype=markdown

" Always use UTF-8
set termencoding=utf-8
set fileencoding=utf-8

set lazyredraw

let g:netrw_banner = 0
let g:netrw_liststyle = 3
" let g:netrw_browse_split = 4
let g:netrw_browse_split = 0
let g:netrw_altv = 1
let g:netrw_winsize = 25 

" Source: http://stackoverflow.com/a/6404246/151007
let i = 1
" If I have more than 9 windows open I have bigger problems ^^
while i <= 9
  execute 'nnoremap <silent> <Leader>'.i.' :'.i.'wincmd w<CR>'
  let i = i + 1
endwhile 
     

" Status Line {
  set laststatus=2 " always show statusbar
  set statusline=
  set statusline+=[%{winnr()}]
  set statusline+=\ Buffer:
  set statusline+=\ %-10.1n\ " buffer number
  set statusline+=%f\ " filename
  set statusline+=%h%m%r%w " status flags
  set statusline+=\[%{strlen(&ft)?&ft:'none'}] "file type
  set statusline+=%= "right align remainder
  set statusline+=0x%-8B " character value
  set statusline+=%-14(%l,%c%V%) " line, character
  set statusline+=%<%P " file position 
" }

colorscheme desert

" remap leader key to something more reachable
let mapleader = ","

" grepper settings
let g:grepper = {}
let g:grepper.tools = ['grep', 'git', 'rg']

" ===============================================
" ===========  Mappings =========================
" ===============================================
nmap <C-p> :FZF<CR>

" Search for the current word
nnoremap <Leader>* :Grepper -cword -noprompt<CR>

" Search for the current selection
nmap gs <plug>(GrepperOperator)
xmap gs <plug>(GrepperOperator)

" easier transition from terminal mode to normal mode
tnoremap <Esc> <C-\><C-n>

" send escape even the escape key is remapped
tnoremap <C-v><Esc> <Esc>

" convenience terminal window movement mappings
tnoremap <M-h> <C-\><C-n><C-w>h
tnoremap <M-j> <C-\><C-n><C-w>j
tnoremap <M-k> <C-\><C-n><C-w>k
tnoremap <M-l> <C-\><C-n><C-w>l

nnoremap <M-h> <C-w>h
nnoremap <M-j> <C-w>j
nnoremap <M-k> <C-w>k
nnoremap <M-l> <C-w>l

inoremap <M-h> <Esc><C-w>h
inoremap <M-j> <Esc><C-w>j
inoremap <M-k> <Esc><C-w>k
inoremap <M-l> <Esc><C-w>l

nnoremap <C-J> o<Esc>
nnoremap <C-H> O<Esc>

" ===============================================

let g:deoplete#enable_at_startup = 1

let g:LanguageClient_serverCommands = {
    \ 'haskell': ['hie', '--lsp'],
    \ }

nnoremap <silent> K :call LanguageClient_textDocument_hover()<CR>
nnoremap <silent> gd :call LanguageClient_textDocument_definition()<CR>
nnoremap <silent> <F2> :call LanguageClient_textDocument_rename()<CR>

if executable('nvr')
  let $VISUAL="nvr -cc split --remote-wait +'set bufhidden=wipe'"
endif

" minpac package manager 
packadd minpac

" ===============================================
" =============== Plugins =======================
" ===============================================

call minpac#init()

call minpac#add('k-takata/minpac', {'type': 'opt'})
call minpac#add('vim-jp/syntax-vim-ex')

call minpac#add('tpope/vim-dispatch')
call minpac#add('tpope/vim-vinegar')
call minpac#add('tpope/vim-unimpaired')
call minpac#add('tpope/vim-commentary')
call minpac#add('tpope/vim-surround')
call minpac#add('tpope/vim-obsession')

call minpac#add('w0rp/ale')
call minpac#add('mhinz/vim-grepper')
call minpac#add('junegunn/fzf')
call minpac#add('janko-m/vim-test')
call minpac#add('sgur/vim-editorconfig')
call minpac#add('autozimu/LanguageClient-neovim', { 'branch': 'next', 'do': {-> system('bash install.sh')}})
call minpac#add('Shougo/deoplete.nvim')
call minpac#add('neovimhaskell/haskell-vim')

" ==============================================
