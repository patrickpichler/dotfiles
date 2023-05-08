" Plugins {{{

call plug#begin('~/.local/share/nvim/plugged')

" ======================================================


Plug 'mbbill/undotree'


" ===========================================

Plug 'https://tildegit.org/sloum/gemini-vim-syntax'
Plug 'catppuccin/nvim', { 'as': 'catppuccin' }

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

" Map Ctrl+c to Esc, as Ctrl+C does not do some usefull things
map <C-c> <Esc>
map! <C-c> <Esc>

" remap leader key to something more reachable
let mapleader = ","

" }}}

" Mappings {{{
nnoremap <silent><leader>ou :UndotreeToggle<CR>
" }}}

" Filetypes {{{

augroup additional_ft
  au!

  autocmd BufNewFile,BufRead Jenkinsfile set ft=groovy

  " terraform
  autocmd FileType terraform-vars set ft=terraform
  autocmd BufNewFile,BufRead *.terraformrc set ft=hcl
  autocmd BufNewFile,BufRead terraform.rc set ft=hcl
  autocmd BufNewFile,BufRead *.tfstate set ft=json
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
" let g:markdown_fenced_languages = ['html', 'java', 'groovy', 'bash=sh',
"       \ 'sh']
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

" vim: foldmethod=marker foldlevel=0 foldenable
