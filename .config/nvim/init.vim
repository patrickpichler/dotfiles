" vim:set foldmethod=marker  foldlevel=0

" Plugins {{{

call plug#begin('~/.local/share/nvim/plugged')

" ======================== Snippts =====================
Plug 'SirVer/ultisnips'
Plug 'honza/vim-snippets'

" ======================================================

Plug 'tpope/vim-dispatch'
Plug 'tpope/vim-unimpaired'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-obsession'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-dotenv'
Plug 'tpope/vim-dadbod'
Plug 'tpope/vim-eunuch'
Plug 'tpope/vim-abolish'

Plug 'Shougo/denite.nvim', { 'tag': '*' }
Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins'}

Plug 'w0rp/ale'
Plug 'mhinz/vim-grepper'
Plug 'janko-m/vim-test'
Plug 'editorconfig/editorconfig-vim'

Plug 'ludovicchabant/vim-gutentags'
Plug 'sheerun/vim-polyglot'

Plug 'mattn/emmet-vim'
Plug 'skywind3000/asyncrun.vim'
Plug 'liuchengxu/vim-which-key'
Plug 'jiangmiao/auto-pairs'
Plug 'junegunn/vim-easy-align'

Plug 'junegunn/fzf'

Plug 'airblade/vim-gitgutter'

Plug 'vimwiki/vimwiki'
Plug 'itchyny/lightline.vim'
Plug 'easymotion/vim-easymotion'

Plug 'scrooloose/nerdtree'
Plug 'tiagofumo/vim-nerdtree-syntax-highlight'

Plug 'mbbill/undotree'

" ============= LSP ===========================
Plug 'neoclide/coc.nvim', {'do': { -> coc#util#install()}}
Plug 'neoclide/coc-denite'

" ============= Coc extensions ===============
Plug 'neoclide/coc-emmet', {'do': 'yarn install --frozen-lockfile'}
Plug 'neoclide/coc-rls', {'do': 'yarn install --frozen-lockfile'}
Plug 'neoclide/coc-tsserver', {'do': 'yarn install --frozen-lockfile'}
Plug 'neoclide/coc-java', {'do': 'yarn install --frozen-lockfile'}
Plug 'iamcco/coc-angular', {'do': 'yarn install --frozen-lockfile'}
Plug 'iamcco/coc-vimlsp', {'do': 'yarn install --frozen-lockfile'}

" ============= New text objects ==============
Plug 'vim-utils/vim-line'
Plug 'wellle/targets.vim'

" ============== HASKELL ======================
Plug 'neovimhaskell/haskell-vim', {'for': 'haskell'}

" ============== ELM ==========================
Plug 'pbogut/deoplete-elm', {'for': 'elm'}
Plug 'ElmCast/elm-vim', {'for': 'elm'}

" ============= JS ============================
Plug 'mxw/vim-jsx', {'for': 'js'}

" ============= Clojure ==========================
Plug 'tpope/vim-salve', { 'for': 'clojure' }
Plug 'tpope/vim-fireplace', { 'for': 'clojure' }

" ============ PlantUML ==========================
Plug 'aklt/plantuml-syntax'
Plug 'tyru/open-browser.vim'
Plug 'weirongxu/plantuml-previewer.vim'

" ===========================================

Plug 'schickele/vim-nachtleben'

" ===========================================
call plug#end()

" }}}

" General configuration {{{
colorscheme nachtleben

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

" Enables search highlighting
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

" " Detect .md as markdown instead of modula-l2
" autocmd BufNewFile,BufReadPost *.md set filetype=markdown

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


" Status Line {
set laststatus=2 " always show statusbar
set noshowmode

" }}}

" Lightline {{{
let g:lightline = {
      \ 'colorscheme': 'jellybeans',
      \ 'active': {
      \   'left': [ [ 'mode', 'paste', ],
      \             [ 'cocerror', 'cocwarn'  ] ,
      \             [ 'gitgutter', 'gitbranch', 'readonly', 'filename', 'modified' ] ]
      \ },
      \ 'component_function': {
      \   'gitbranch': 'fugitive#head',
      \   'readonly': 'LightlineReadonly',
      \   'gitgutter': 'LightLineGitGutter',
      \   'filename': 'LightlineFileName',
      \ },
      \ 'component_expand': {
      \   'cocerror': 'LightLineCocError',
      \   'cocwarn' : 'LightLineCocWarn',   
      \ }
    \ }

function! LightlineFileName() abort
    let filename = winwidth(0) > 70 ? expand('%') : expand('%:t')
    let modified = &modified ? ' +' : ''
    return fnamemodify(filename, ":~:.") . modified
endfunction

function! LightlineReadonly()
  return &readonly && &filetype !=# 'help' ? 'RO' : ''
endfunction

function! LightLineCocError()
  let error_sign = get(g:, 'coc_status_error_sign', has('mac') ? '❌ ' : 'E')
  let info = get(b:, 'coc_diagnostic_info', {})
  if empty(info)
    return ''
  endif
  let errmsgs = []
  if get(info, 'error', 0)
    call add(errmsgs, error_sign . info['error'])
  endif
  return trim(join(errmsgs, ' ') . ' ' . get(g:, 'coc_status', ''))
endfunction

function! LightLineCocWarn() abort
  let warning_sign = get(g:, 'coc_status_warning_sign')
  let info = get(b:, 'coc_diagnostic_info', {})
  if empty(info)
    return ''
  endif
  let warnmsgs = []
  if get(info, 'warning', 0)
    call add(warnmsgs, warning_sign . info['warning'])
  endif
  return trim(join(warnmsgs, ' ') . ' ' . get(g:, 'coc_status', ''))
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

" grepper settings
let g:grepper = {}
let g:grepper.tools = ['grep', 'git', 'rg']

" Define prefix dictionary
let g:which_key_map =  {}

" coc.nvim {{{
autocmd FileType json syntax match Comment +\/\/.\+$+

inoremap <silent><expr> <c-space> coc#refresh()
nmap <silent> [c <Plug>(coc-diagnostic-prev)
nmap <silent> ]c <Plug>(coc-diagnostic-next)
nmap <silent> [k :CocPrev<cr>
nmap <silent> ]k :CocNext<cr>
nnoremap <silent> K :call <SID>show_documentation()<CR>

nmap <silent> gd <Plug>(coc-definition)
nmap <silent> gy <Plug>(coc-type-definition)
nmap <silent> gi <Plug>(coc-implementation)
nmap <silent> gr <Plug>(coc-references)
nmap <leader>u <Plug>(coc-references)
nmap <leader>rn <Plug>(coc-rename)

function! s:show_documentation()
  if &filetype == 'vim'
    execute 'h '.expand('<cword>')
  else
    call CocAction('doHover')
  endif
endfunction

" Highlight symbol under cursor on CursorHold
autocmd CursorHold * silent call CocActionAsync('highlight')

" Remap for format selected region
xmap <leader>f  <Plug>(coc-format-selected)
nmap <leader>f  <Plug>(coc-format-selected)

" Remap for do codeAction of selected region, ex: `<leader>aap` for current paragraph
xmap <leader>a  <Plug>(coc-codeaction-selected)
nmap <leader>a  <Plug>(coc-codeaction-selected)

" Remap for do codeAction of current line
nmap <leader>ac  <Plug>(coc-codeaction)
" Fix autofix problem of current line
nmap <leader>qf  <Plug>(coc-fix-current)

" Use `:Format` to format current buffer
command! -nargs=0 Format :call CocAction('format')

" Use `:Fold` to fold current buffer
command! -nargs=? Fold :call     CocAction('fold', <f-args>)

" Using CocList
" Show all diagnostics
nnoremap <silent> <space>a  :<C-u>CocList diagnostics<cr>
" Manage extensions
nnoremap <silent> <space>e  :<C-u>CocList extensions<cr>
" Show commands
nnoremap <silent> <space>c  :<C-u>CocList commands<cr>
" Find symbol of current document
nnoremap <silent> <space>o  :<C-u>CocList outline<cr>
" Search workspace symbols
nnoremap <silent> <space>s  :<C-u>CocList -I symbols<cr>
" Do default action for next item.
nnoremap <silent> <space>j  :<C-u>CocNext<CR>
" Do default action for previous item.
nnoremap <silent> <space>k  :<C-u>CocPrev<CR>
" Resume latest coc list
nnoremap <silent> <space>p  :<C-u>CocListResume<CR>

" }}}

" Mappings {{{
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

" Make C-P C-N behave the same as Up Down in command mode
cnoremap <C-P> <Up>
cnoremap <C-N> <Down>

" ======== Denite ===========================
let g:which_key_map.d = { 'name' : '+denite' }
let g:which_key_map.d.b = 'buffers'
let g:which_key_map.d.f = 'file rec'
let g:which_key_map.d.g = 'grep'


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

let g:deoplete#enable_at_startup = 1

" ============ Vim which key ====================
nnoremap <silent> <Leader> :<c-u>WhichKey ','<CR>

autocmd! FileType which_key
autocmd  FileType which_key set laststatus=0 noshowmode noruler
      \| autocmd BufLeave <buffer> set laststatus=2 showmode ruler

" }}}

" Colorscheme {{{
hi ALEError ctermfg=Red

let g:ale_completion_enabled = 0
let g:ale_sign_column_always = 1

let g:ale_sign_error = '>' 
let g:ale_sign_warning = '.'

highlight GitGutterAdd    ctermfg=green guifg=green
highlight GitGutterChange ctermfg=yellow guifg=yellow
highlight GitGutterDelete ctermfg=red guifg=red

" }}}

" Snippets {{{

" Trigger configuration. Do not use <tab> if you use https://github.com/Valloric/YouCompleteMe.
let g:UltiSnipsExpandTrigger="<tab>"
let g:UltiSnipsJumpForwardTrigger="<c-l>"
let g:UltiSnipsJumpBackwardTrigger="<c-h>"

" If you want :UltiSnipsEdit to split your window.
let g:UltiSnipsEditSplit="vertical"

 " }}}

" Easy align {{{

" Start interactive EasyAlign in visual mode (e.g. vipga)
xmap ga <Plug>(EasyAlign)

" Start interactive EasyAlign for a motion/text object (e.g. gaip)
nmap ga <Plug>(EasyAlign)

" }}}

" Vim Wiki {{{

" Use markdown syntax
let g:vimwiki_list = [{'path': '~/vimwiki/',
                      \ 'syntax': 'markdown', 'ext': '.wikimd'}]

" Auto-commit

if executable("git") && isdirectory($HOME . "/vimwiki/.git")

  augroup wiki
    au! BufWritePost ~/vimwiki/* !cd ~/vimwiki/;git add "%:p";git commit -m "Auto commit of %:t." "%:p"
  augroup END

endif

" }}}

" Filetypes {{{

augroup additional_ft
  au!
  
  autocmd BufNewFile,BufRead Jenkinsfile set ft=groovy
  autocmd BufNewFile,BufRead *.ts set ft=typescript
augroup END

" }}}

" GutenTags {{{

" generate datebases in my cache directory, prevent gtags files polluting my project
let g:gutentags_cache_dir = expand('~/.cache/tags')

" }}}

" Markdown {{{
let g:markdown_fenced_languages = ['html', 'java', 'groovy', 'bash=sh', 
      \ 'sh', 'kotlin']
" }}}

" Polyglot {{{

let g:polyglot_disabled = ['markdown', 'clojure']

" }}}

" Java {{{

let java_highlight_functions = 1
let java_highlight_all = 1

augroup java_stuff
  au!
  
  autocmd Filetype java call SetJavaOptions()
  function SetJavaOptions()
    highlight link javaScopeDecl Statement
    highlight link javaType Type 
    highlight link javaDocTags PreProc
  endfunction

augroup END

" }}}

" UndoTree {{{
nnoremap <silent><Leader>tu :UndotreeToggle
" }}}


" Denite {{{

let s:menus = {}

call denite#custom#var('menu', 'menus', s:menus)

nnoremap <silent><Leader>db :Denite buffer<CR>
nnoremap <silent><Leader>df :Denite file/rec<CR>
nnoremap <silent><Leader>dg :Denite grep<CR>

nnoremap <silent><M-b> :Denite buffer<CR>
nnoremap <silent><M-f> :Denite file/rec<CR>
nnoremap <silent><M-g> :Denite grep<CR>

call denite#custom#option('_', {
    \ 'cached_filter': v:true,
    \ 'cursor_shape': v:true,
    \ 'cursor_wrap': v:true,
    \ 'highlight_filter_background': 'DeniteFilter',
    \ 'highlight_matched_char': 'Underlined',
    \ 'matchers': 'matcher/fruzzy',
    \ 'statusline': v:false,
    \ })

" }}}

" Ripgrep {{{
if executable('rg')
	" Ripgrep
  call denite#custom#var('file/rec', 'command',
        \ ['rg', '--files', '--glob', '!.git'])
  call denite#custom#var('grep', 'command', ['rg', '--threads', '1'])
  call denite#custom#var('grep', 'recursive_opts', [])
  call denite#custom#var('grep', 'final_opts', [])
  call denite#custom#var('grep', 'separator', ['--'])
  call denite#custom#var('grep', 'default_opts',
        \ ['-i', '--vimgrep', '--no-heading'])
endif

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
      " find the current file in nerdtree without needing to reload the drawer
      nmap <silent> <leader>y :NERDTreeFind<cr>
      nmap <silent> - :NERDTreeFind<cr>

      let NERDTreeShowHidden=1
      let g:NERDTreeFileExtensionHighlightFullName = 1
      let g:NERDTreeExactMatchHighlightFullName = 1
      let g:NERDTreePatternMatchHighlightFullName = 1
" }}}

" Vim config {{{

if exists("$vim_mode")
  execute 'source' fnamemodify(expand('<sfile>'), ':h').'/config/'.$vim_mode.'.vim'
endif

" }}}

" Is at the end so that specializations can insert things too 
call which_key#register(',', "g:which_key_map")

