
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

Plug 'arthurxavierx/vim-caser'

Plug 'editorconfig/editorconfig-vim'

Plug 'mattn/emmet-vim'
Plug 'skywind3000/asyncrun.vim'
Plug 'cohama/lexima.vim'

Plug 'junegunn/rainbow_parentheses.vim'

Plug 'airblade/vim-gitgutter'

Plug 'itchyny/lightline.vim'
Plug 'easymotion/vim-easymotion'

Plug 'scrooloose/nerdtree'

Plug 'AndrewRadev/linediff.vim'
Plug 'AndrewRadev/inline_edit.vim'

Plug 'mbbill/undotree'

Plug 'Shougo/neosnippet.vim'
Plug 'Shougo/neosnippet-snippets'

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

" Plug 'Shougo/denite.nvim', { 'do': ':UpdateRemotePlugins', 'tag': '*' }
Plug 'Shougo/denite.nvim', { 'do': ':UpdateRemotePlugins', 'commit': 'db2d82cfbd85d8b6caafbd967a27f4d1c6ea5fa6' }
Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
Plug 'lighttiger2505/deoplete-vim-lsp'

" ============= lsp ==============
Plug 'patrickpichler/vim-lsp'
Plug 'mattn/vim-lsp-settings'

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

set completeopt=noinsert,noselect,menuone
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

set undodir^=~/.local/share/nvim/undo//

" }}}

" Auto open quickfix list on grep
autocmd QuickFixCmdPost *grep* cwindow

" autohighlight word under cursor

set updatetime=10


" Status Line {
set laststatus=2 " always show statusbar
set noshowmode

" remap leader key to something more reachable
let mapleader = ","

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

" Deoplete {{{
let g:deoplete#enable_at_startup = 1

"Autocomplete and cycle from top-to-bottom of suggestions using <Tab>.
inoremap <expr><TAB> pumvisible() ? "\<c-n>" : "\<TAB>"

"<TAB>: completion.
inoremap <silent><expr> <TAB>
      \ pumvisible() ? "\<C-n>" :
      \ <SID>check_back_space() ? "\<TAB>" :
      \ deoplete#manual_complete()
function! s:check_back_space() abort
  let col = col('.') - 1
  return !col || getline('.')[col - 1]  =~ '\s'
endfunction

"<S-TAB>: completion back.
inoremap <expr><S-TAB>  pumvisible() ? "\<C-p>" : "\<C-h>"

"<CR>: close popup and save indent.
inoremap <silent> <CR> <C-r>=<SID>my_cr_function()<CR>
function! s:my_cr_function() abort
  return deoplete#cancel_popup() . "\<CR>"
endfunction

call deoplete#custom#option('camel_case', v:true)
call deoplete#custom#option('auto_complete_delay', 0)
call deoplete#custom#option('smart_case', v:true)
call deoplete#custom#option('min_pattern_length', 1)
"}}}

" Neosnip {{{
" Plugin key-mappings.
" Note: It must be "imap" and "smap".  It uses <Plug> mappings.
imap <C-k>     <Plug>(neosnippet_expand_or_jump)
smap <C-k>     <Plug>(neosnippet_expand_or_jump)
xmap <C-k>     <Plug>(neosnippet_expand_target)

" SuperTab like snippets behavior.
" Note: It must be "imap" and "smap".  It uses <Plug> mappings.
imap <expr><TAB>
   \ pumvisible() ? "\<C-n>" :
   \ neosnippet#expandable_or_jumpable() ?
   \    "\<Plug>(neosnippet_expand_or_jump)" : "\<TAB>"

"}}}

" Denite {{{

if executable('rg')
  call denite#custom#var('grep', {
    \ 'command': ['rg'],
    \ 'default_opts': ['-i', '--vimgrep', '--no-heading', '-F'],
    \ 'recursive_opts': [],
    \ 'pattern_opt': [],
    \ 'separator': ['--'],
    \ 'final_opts': [],
    \ })
endif

if executable('fd')
   call denite#custom#var('file/rec', 'command',
        \ ['fd', '--type', 'f', '--follow', '--hidden', '--exclude', '.git',
        \ ''])
    call denite#custom#var('directory_rec', 'command',
        \ ['fd', '--type', 'd', '--follow', '--hidden', '--exclude', '.git',
        \ ''])

   call denite#custom#alias('source', 'file/rec/noignore', 'file/rec')

   call denite#custom#var('file/rec/noignore', 'command',
        \ ['fd', '--type', 'f', '--follow', '--hidden', '--exclude', '.git',
        \ '--no-ignore', ''])
endif

nnoremap <silent><M-b> :Denite buffer<CR>
nnoremap <silent><M-p> :Denite file/rec<CR>
nnoremap <silent><space>g :Denite grep:::!<CR>
nnoremap <silent><M-g> :Denite -buffer-name=grep -default-action=quickfix grep:::!<CR>
nnoremap <silent><leader>* :DeniteCursorWord -buffer-name=grep -default-action=quickfix grep<CR>

call denite#custom#option('_', {
    \ 'cached_filter': v:true,
    \ 'cursor_shape': v:true,
    \ 'cursor_wrap': v:true,
    \ 'highlight_filter_background': 'DeniteFilter',
    \ 'highlight_matched_char': 'Underlined',
    \ 'matchers': 'matcher/fuzzy',
    \ 'sorters': 'sorter/sublime',
    \ 'statusline': v:false,
    \ 'start_filter': v:true,
    \ })

" Define mappings
autocmd FileType denite call s:denite_my_settings()
function! s:denite_my_settings() abort
  nnoremap <silent><buffer><expr> <CR>
  \ denite#do_map('do_action')
  nnoremap <silent><buffer><expr> v
  \ denite#do_map('do_action', 'vsplit')
  nnoremap <silent><buffer><expr> d
  \ denite#do_map('do_action', 'delete')
  nnoremap <silent><buffer><expr> p
  \ denite#do_map('do_action', 'preview')
  nnoremap <silent><buffer><expr> q
  \ denite#do_map('quit')
  nnoremap <silent><buffer><expr> l
  \ denite#do_map('do_action', 'quickfix')
  nnoremap <silent><buffer><expr> i
  \ denite#do_map('open_filter_buffer')
  nnoremap <silent><buffer><expr> s
  \ denite#do_map('toggle_select')
  nnoremap <silent><buffer><expr> a
  \ denite#do_map('toggle_select_all')
endfunction

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

" }}}

" NERDTree {{{
      augroup nerdtree_group
          autocmd!
          autocmd FileType nerdtree setlocal nolist " turn off whitespace characters
          autocmd FileType nerdtree setlocal nocursorline " turn off line highlighting for performance
          autocmd StdinReadPre * let s:std_in=1
          autocmd VimEnter * if argc() == 1 && isdirectory(argv()[0]) && !exists("s:std_in") | exe 'NERDTree' argv()[0] | wincmd p | ene | exe 'cd '.argv()[0] | endif
      augroup END

      " Toggle NERDTree
      function! ToggleNerdTree()
          if @% != "" && @% !~ "Startify" && (!exists("g:NERDTree") || (g:NERDTree.ExistsForTab() && !g:NERDTree.IsOpen()))
              :NERDTreeFind
          else
              :NERDTreeToggle
          endif
      endfunction

      " toggle nerd tree
      nmap <silent> <leader>k :call ToggleNerdTree()<cr>
      nmap <silent> - :NERDTreeFind<cr>

      let NERDTreeShowHidden=1
      let g:NERDTreeFileExtensionHighlightFullName = 1
      let g:NERDTreeExactMatchHighlightFullName = 1
      let g:NERDTreePatternMatchHighlightFullName = 1
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

" GitGutter {{{

nmap [h <Plug>(GitGutterPrevHunk)
nmap ]h <Plug>(GitGutterNextHunk)

highlight SignColumn guibg=#ECECEC
highlight GitGutterChange guifg=#FF8000 ctermfg=3

omap ic <Plug>(GitGutterTextObjectInnerPending)
omap ac <Plug>(GitGutterTextObjectOuterPending)
xmap ic <Plug>(GitGutterTextObjectInnerVisual)
xmap ac <Plug>(GitGutterTextObjectOuterVisual)

" }}}

" vim-commentary {{{
autocmd FileType asm setlocal commentstring=;\ %s
"}}}

" vim-lsp {{{

function! s:on_lsp_buffer_enabled() abort
  setlocal omnifunc=lsp#complete
  setlocal signcolumn=yes
  if exists('+tagfunc') | setlocal tagfunc=lsp#tagfunc | endif
  nmap <buffer> gd <plug>(lsp-definition)
  nmap <buffer> gs <plug>(lsp-document-symbol-search)
  nmap <buffer> gS <plug>(lsp-workspace-symbol-search)
  nmap <buffer> gr <plug>(lsp-references)
  nmap <buffer> gi <plug>(lsp-implementation)
  nmap <buffer> gt <plug>(lsp-type-definition)
  nmap <buffer> <leader>rn <plug>(lsp-rename)
  nmap <buffer> [d <plug>(lsp-previous-diagnostic)
  nmap <buffer> ]d <plug>(lsp-next-diagnostic)
  nmap <buffer> K <plug>(lsp-hover)
  inoremap <buffer> <expr><c-f> lsp#scroll(+4)
  inoremap <buffer> <expr><c-d> lsp#scroll(-4)

  let g:lsp_format_sync_timeout = 1000
  autocmd! BufWritePre *.rs,*.go call execute('LspDocumentFormatSync')

  set foldmethod=expr
        \ foldexpr=lsp#ui#vim#folding#foldexpr()
        \ foldtext=lsp#ui#vim#folding#foldtext()

  autocmd User lsp_float_opened nnoremap <buffer> <expr><c-f> lsp#scroll(+5)
  autocmd User lsp_float_opened nnoremap <buffer> <expr><c-d> lsp#scroll(-5)
  autocmd User lsp_float_closed nunmap <buffer> <c-f>
  autocmd User lsp_float_closed nunmap <buffer> <c-d>

  " refer to doc to add more commands
endfunction

augroup lsp_install
  au!
  " call s:on_lsp_buffer_enabled only for languages that has the server registered.
  autocmd User lsp_buffer_enabled call s:on_lsp_buffer_enabled()
augroup END

" For conceal markers.
if has('conceal')
  set conceallevel=2 concealcursor=niv
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
