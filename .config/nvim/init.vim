" Used Vim settings, rather than Vi settings (much better!).
" This must be first, because it changes other options as a side effect.
set nocompatible

" Make backspace behave in a sane manner.
"
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

set timeoutlen=500

set inccommand=split 

" Detect .md as markdown instead of modula-l2
autocmd BufNewFile,BufReadPost *.md set filetype=markdown

" Always use UTF-8
set termencoding=utf-8
set fileencoding=utf-8

set lazyredraw

" Auto open quickfix list on grep
autocmd QuickFixCmdPost *grep* cwindow

" Netrw config
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

" autohighlight word under cursor

set updatetime=10

function! HighlightWordUnderCursor()
  if getline(".")[col(".")-1] !~# '[[:punct:][:blank:]]' 
    exec 'match' 'Search' '/\V\<'.expand('<cword>').'\>/' 
  else 
    match none 
  endif
endfunction

" autocmd! CursorHold,CursorHoldI * call HighlightWordUnderCursor()

" Status Line {
set laststatus=2 " always show statusbar
set statusline=
set statusline+=[%{winnr()}]
set statusline+=\ Buffer:
set statusline+=\ %-10.1n\ " buffer number

set statusline+=%f\ " filename
set statusline+=%h%m%r%w " status flags
set statusline+=\[%{strlen(&ft)?&ft:'none'}] "file type
set statusline+=%{gutentags#statusline('[',']')}
set statusline+=%= "right align remainder
set statusline+=\ %{fugitive#statusline()}
set statusline+=\ 0x%-8B " character value
set statusline+=%-14(%l,%c%V%) " line, character
set statusline+=%<%P " file position 
" }

colorscheme desert

" remap leader key to something more reachable
let mapleader = ","

" grepper settings
let g:grepper = {}
let g:grepper.tools = ['grep', 'git', 'rg']

" Define prefix dictionary
let g:which_key_map =  {}

" ===============================================
" ===========  Mappings =========================
" ===============================================
nmap <C-p> :FZF<CR>

" Search for the current word
nnoremap <Leader>* :Grepper -cword -noprompt<CR>

" Search for the current selection
nmap gs <Plug>GrepperOperator
xmap gs <Plug>GrepperOperator

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

" ======== Denite ===========================
let g:which_key_map.d = { 'name' : '+denite' }
let g:which_key_map.d.b = 'buffers'
let g:which_key_map.d.f = 'file rec'
let g:which_key_map.d.g = 'grep'

nnoremap <silent><Leader>db :Denite buffer<CR>
nnoremap <silent><Leader>df :Denite file/rec<CR>
nnoremap <silent><Leader>dg :Denite grep<CR>

nnoremap <silent><M-b> :Denite buffer<CR>
nnoremap <silent><M-f> :Denite file/rec<CR>
nnoremap <silent><M-g> :Denite grep<CR>


" ======== Buffers ==========================
let g:which_key_map.b = { 'name' : '+buffers' }
let g:which_key_map.b.d = 'delete'
let g:which_key_map.b.o = 'only'

nnoremap <silent><Leader>bd :bd<CR>
nnoremap <silent><Leader>bo :only<CR>

" ======== highlighting  ====================
let g:which_key_map.h = { 'name' : '+highlight' }
let g:which_key_map.h.w = 'highlight word'
let g:which_key_map.h.c = 'clear word highlighting'
let g:which_key_map.h.n = 'clear word highlighting'

nnoremap <silent><Leader>hw :call HighlightWordUnderCursor()<CR>
nnoremap <silent><Leader>hc :match none<CR>
nnoremap <silent><Leader>hn :noh<CR>

" ======== file group ======================
let g:which_key_map.f = { 'name' : '+file' }
let g:which_key_map.f.s = 'save-file'

" more convenient save
nnoremap <silent><Leader>fs :w<CR>

" =========================================

nnoremap <silent> <leader>oq  :copen<CR>
nnoremap <silent> <leader>ol  :lopen<CR>
let g:which_key_map.o = {
      \ 'name' : '+open',
      \ 'q' : 'open-quickfix'    ,
      \ 'l' : 'open-locationlist',
      \ }

" this allows to do :w!! for overwriting readonly files 
cnoremap w!! execute 'silent! write !sudo tee % >/dev/null' <bar> edit!
" ===============================================

nnoremap <F5> :call LanguageClient_contextMenu()<CR>

nnoremap <silent> K :call LanguageClient_textDocument_hover()<CR>
nnoremap <silent> gd :call LanguageClient_textDocument_definition()<CR>
nnoremap <silent> <F2> :call LanguageClient_textDocument_rename()<CR>

" ============ Vim which key ====================
nnoremap <silent> <Leader> :<c-u>WhichKey ','<CR>

autocmd! FileType which_key
autocmd  FileType which_key set laststatus=0 noshowmode noruler
      \| autocmd BufLeave <buffer> set laststatus=2 showmode ruler

" ===============================================

" ===============================================
" ============ Colorscheme ======================
" ===============================================
hi ALEError ctermfg=Red

let g:ale_completion_enabled = 1
let g:ale_sign_column_always = 1

let g:ale_sign_error = '●' " Less aggressive than the default '>>'
let g:ale_sign_warning = '.'


" ===============================================
" ============== NCM2 ===========================
" ===============================================

" enable ncm2 for all buffers
autocmd BufEnter * call ncm2#enable_for_buffer()

" IMPORTANTE: :help Ncm2PopupOpen for more information
set completeopt=noinsert,menuone,noselect
set shortmess+=c

au TextChangedI * call ncm2#auto_trigger()

inoremap <c-c> <ESC>

inoremap <expr> <CR> (pumvisible() ? "\<c-y>\<cr>" : "\<CR>")
inoremap <expr> <Tab> pumvisible() ? "\<C-n>" : "\<Tab>"
inoremap <expr> <S-Tab> pumvisible() ? "\<C-p>" : "\<S-Tab>"

" ===============================================
" ============== Snippets =======================
" ===============================================

" Trigger configuration. Do not use <tab> if you use https://github.com/Valloric/YouCompleteMe.
let g:UltiSnipsExpandTrigger="<tab>"
let g:UltiSnipsJumpForwardTrigger="<c-l>"
let g:UltiSnipsJumpBackwardTrigger="<c-h>"

" If you want :UltiSnipsEdit to split your window.
let g:UltiSnipsEditSplit="vertical"

" ===============================================
" ============ Easy align =======================
" ===============================================

" Start interactive EasyAlign in visual mode (e.g. vipga)
xmap ga <Plug>(EasyAlign)

" Start interactive EasyAlign for a motion/text object (e.g. gaip)
nmap ga <Plug>(EasyAlign)

" ===============================================
" =============== Plugins =======================
" ===============================================

" minpac package manager 
packadd minpac

call minpac#init()

call minpac#add('k-takata/minpac', {'type': 'opt'})

" ========================== NCM2 ======================
call minpac#add('ncm2/ncm2')
call minpac#add('roxma/nvim-yarp')
call minpac#add('ncm2/ncm2-bufword')
call minpac#add('ncm2/ncm2-path')
call minpac#add('ncm2/ncm2-tmux')
call minpac#add('ncm2/ncm2-tagprefix')
call minpac#add('ncm2/ncm2-ultisnips')
call minpac#add('ncm2/ncm2-gtags')
call minpac#add('wellle/tmux-complete.vim')


" ======================== Snippts =====================
call minpac#add('SirVer/ultisnips')
call minpac#add('honza/vim-snippets')

" ======================================================

call minpac#add('tpope/vim-dispatch')
call minpac#add('tpope/vim-vinegar')
call minpac#add('tpope/vim-unimpaired')
call minpac#add('tpope/vim-commentary')
call minpac#add('tpope/vim-surround')
call minpac#add('tpope/vim-obsession')
call minpac#add('tpope/vim-fugitive')

call minpac#add('Shougo/denite.nvim')

call minpac#add('w0rp/ale')
call minpac#add('mhinz/vim-grepper')
call minpac#add('janko-m/vim-test')
call minpac#add('sgur/vim-editorconfig')
call minpac#add('autozimu/LanguageClient-neovim', {'branch' : 'next' , 'do': {-> system('bash install.sh')}})
call minpac#add('ludovicchabant/vim-gutentags')
call minpac#add('sheerun/vim-polyglot', {'type': 'opt'})

call minpac#add('mattn/emmet-vim')
call minpac#add('skywind3000/asyncrun.vim')
call minpac#add('liuchengxu/vim-which-key')
call minpac#add('jiangmiao/auto-pairs')
call minpac#add('junegunn/vim-easy-align')

" ============== HASKELL ======================
call minpac#add('neovimhaskell/haskell-vim', {'type': 'opt'})

" ============== ELM ==========================
call minpac#add('ElmCast/elm-vim', {'type': 'opt'})

" ============= JS ============================
call minpac#add('mxw/vim-jsx', {'type': 'opt'})

" ============= Julia ============================
call minpac#add('JuliaEditorSupport/julia-vim', {'type': 'opt'})
" ==============================================

packloadall

" ====== Denite =============

" Ripgrep command on grep source
call denite#custom#var('grep', 'command', ['rg'])
call denite#custom#var('grep', 'default_opts',
      \ ['--vimgrep', '--no-heading'])
call denite#custom#var('grep', 'recursive_opts', [])
call denite#custom#var('grep', 'pattern_opt', ['--regexp'])
call denite#custom#var('grep', 'separator', ['--'])
call denite#custom#var('grep', 'final_opts', [])

call denite#custom#var('file/rec', 'command',
      \ ['fd', '--full-path'])

" ==== Load vim config =====

if exists("$vim_mode")
  execute 'source' fnamemodify(expand('<sfile>'), ':h').'/config/'.$vim_mode.'.vim'
endif

" Is at the end so that specializations can insert things too 
call which_key#register(',', "g:which_key_map")
