
" Plugins {{{

call plug#begin('~/.local/share/nvim/plugged')

" ======================================================

Plug 'tpope/vim-dispatch'
Plug 'tpope/vim-unimpaired'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-obsession'
Plug 'tpope/vim-fugitive', { 'tag': '*' }
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-dotenv'
Plug 'tpope/vim-dadbod'
Plug 'tpope/vim-eunuch'
Plug 'tpope/vim-projectionist'

Plug 'arthurxavierx/vim-caser'

Plug 'Shougo/denite.nvim', { 'tag': '*' }

Plug 'mhinz/vim-grepper'
Plug 'janko/vim-test'
Plug 'editorconfig/editorconfig-vim'

Plug 'mattn/emmet-vim'
Plug 'skywind3000/asyncrun.vim'
Plug 'liuchengxu/vim-which-key'
Plug 'cohama/lexima.vim'

Plug 'junegunn/vim-easy-align'
Plug 'junegunn/rainbow_parentheses.vim'

Plug 'airblade/vim-gitgutter'

Plug 'vimwiki/vimwiki'
Plug 'itchyny/lightline.vim'
Plug 'easymotion/vim-easymotion'

Plug 'scrooloose/nerdtree'
Plug 'tiagofumo/vim-nerdtree-syntax-highlight'

Plug 'AndrewRadev/linediff.vim'
Plug 'AndrewRadev/inline_edit.vim'

Plug 'mbbill/undotree'

Plug 'honza/vim-snippets'

Plug 'AndrewRadev/bufferize.vim'
Plug 'tommcdo/vim-exchange'
Plug 'junegunn/vim-peekaboo'

Plug 'cespare/vim-toml', { 'for': 'toml' }
Plug 'udalov/kotlin-vim', { 'for': 'kotlin' }
Plug 'ekalinin/Dockerfile.vim', { 'for': 'dockerfile' }

Plug 'blankname/vim-fish', { 'for': 'fish' }

" ============= LSP ===========================
Plug 'neoclide/coc.nvim', {'tag': '*', 'branch': 'release'}

" ============= Coc extensions ===============
Plug 'neoclide/coc-emmet', {'tag': '*', 'do': 'yarn install --frozen-lockfile'}
Plug 'neoclide/coc-rls', {'tag': '*', 'do': 'yarn install --frozen-lockfile'}
Plug 'neoclide/coc-java', {'tag': '*', 'do': 'yarn install --frozen-lockfile'}
Plug 'neoclide/coc-json', {'tag': '*', 'do': 'yarn install --frozen-lockfile'}
Plug 'neoclide/coc-tsserver', {'tag': '*', 'do': 'yarn install --frozen-lockfile'}
Plug 'neoclide/coc-tslint-plugin', {'tag': '*', 'do': 'yarn install --frozen-lockfile'}
Plug 'neoclide/coc-html', {'tag': '*', 'do': 'yarn install --frozen-lockfile'}
Plug 'neoclide/coc-css', {'tag': '*', 'do': 'yarn install --frozen-lockfile'}
Plug 'neoclide/coc-snippets', {'tag': '*', 'do': 'yarn install --frozen-lockfile'}
Plug 'neoclide/coc-python', {'tag': '*', 'do': 'yarn install --frozen-lockfile'}
Plug 'neoclide/coc-lists', {'tag': '*', 'do': 'yarn install --frozen-lockfile'}

" ============= New text objects ==============
Plug 'kana/vim-textobj-user'
Plug 'vim-utils/vim-line'
Plug 'wellle/targets.vim'
Plug 'kana/vim-textobj-entire'

" ============== HASKELL ======================
Plug 'neovimhaskell/haskell-vim', {'for': 'haskell'}

" ============== ELM ==========================
Plug 'ElmCast/elm-vim', {'for': 'elm'}

" ============= JS ============================
Plug 'mxw/vim-jsx', {'for': 'js'}

" ============= Clojure ==========================
Plug 'tpope/vim-salve', { 'for': 'clojure' }
Plug 'tpope/vim-fireplace', { 'for': 'clojure' }

" ============= Crystal ==========================
Plug 'rhysd/vim-crystal', { 'for': 'crystal' }

" ============ PlantUML ==========================
Plug 'aklt/plantuml-syntax'
Plug 'tyru/open-browser.vim'
Plug 'weirongxu/plantuml-previewer.vim'

" ===========================================
Plug 'leafgarland/typescript-vim'

Plug 'https://git.sr.ht/~patrickpichler/vim-nachtleben-pitch-black'

" ===========================================
call plug#end()

" }}}

" General configuration {{{
colorscheme nachtleben-pitch-black

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

set timeoutlen=500

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

inoremap <silent><expr> <TAB>
      \ pumvisible() ? "\<C-n>" :
      \ <SID>check_back_space() ? "\<TAB>" :
      \ coc#refresh()
inoremap <expr><S-TAB> pumvisible() ? "\<C-p>" : "\<C-h>"

function! s:check_back_space() abort
  let col = col('.') - 1
  return !col || getline('.')[col - 1]  =~# '\s'
endfunction

inoremap <silent><expr> <c-space> coc#refresh()
nmap <silent> [d <Plug>(coc-diagnostic-prev)
nmap <silent> ]d <Plug>(coc-diagnostic-next)
nmap <silent> [k :CocPrev<cr>
nmap <silent> ]k :CocNext<cr>
nnoremap <silent> K :call <SID>show_documentation()<CR>

nmap <silent> gd <Plug>(coc-definition)
nmap <silent> gy <Plug>(coc-type-definition)
nmap <silent> gi <Plug>(coc-implementation)
nmap <silent> gr <Plug>(coc-references)
nmap <leader>u <Plug>(coc-references)
nmap <leader>rn <Plug>(coc-rename)

" CocSnippet {{{
" Use <C-l> for trigger snippet expand.
imap <C-l> <Plug>(coc-snippets-expand)

" Use <C-j> for select text for visual placeholder of snippet.
vmap <C-j> <Plug>(coc-snippets-select)

" Use <C-j> for jump to next placeholder, it's default of coc.nvim
let g:coc_snippet_next = '<c-j>'

" Use <C-k> for jump to previous placeholder, it's default of coc.nvim
let g:coc_snippet_prev = '<c-k>'

" Use <C-j> for both expand and jump (make expand higher priority.)
imap <C-j> <Plug>(coc-snippets-expand-jump)

" }}}

inoremap <expr> <cr> pumvisible() ? "\<C-y>" : "\<C-g>u\<CR>"

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

nmap <leader>fa  :<C-u>call CocAction('format')<cr>

" Remap for do codeAction of selected region, ex: `<leader>aap` for current paragraph
xmap <leader>a  <Plug>(coc-codeaction-selected)
nmap <leader>a  <Plug>(coc-codeaction-selected)

" Remap for do codeAction of current line
nmap <leader>ac  <Plug>(coc-codeaction)
" Fix autofix problem of current line
nmap <leader>qf  <Plug>(coc-fix-current)
nmap <silent><leader>p  :call CocActionAsync('showSignatureHelp')<cr>
inoremap <silent><C-p> <C-o>:call CocActionAsync('showSignatureHelp')<cr>

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

" Create mappings for function text object, requires document symbols feature of languageserver.
xmap if <Plug>(coc-funcobj-i)
xmap af <Plug>(coc-funcobj-a)
omap if <Plug>(coc-funcobj-i)
omap af <Plug>(coc-funcobj-a)

" Use <tab> for select selections ranges, needs server support, like: coc-tsserver, coc-python
xmap <silent> <TAB> <Plug>(coc-range-select)
xmap <silent> <S-TAB> <Plug>(coc-range-select-backword)

hi CocWarningFloat ctermbg=130 ctermfg=black guibg=#ff922b guifg=black
hi CocErrorFloat ctermbg=9 ctermfg=black guibg=#ff0000 guifg=black
hi CocInfoFloat ctermbg=11 ctermfg=black guibg=#fab005 guifg=black
hi CocHintFloat ctermbg=12 ctermfg=black guifg=#15aabf guifg=black

" }}}

" Mappings {{{

" Search for the current word
nnoremap <leader>* :Grepper -cword -noprompt<CR>

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

nnoremap <C-h> h
nnoremap <C-j> gj
nnoremap <C-k> gk
nnoremap <C-l> l

" Make C-P C-N behave the same as Up Down in command mode
cnoremap <C-P> <Up>
cnoremap <C-N> <Down>

" merge tool bindings
nnoremap <silent> [g :diffget //2<CR>
nnoremap <silent> ]g :diffget //3<CR>

" ======== Denite ===========================
let g:which_key_map.d = { 'name' : '+denite' }
let g:which_key_map.d.b = 'buffers'
let g:which_key_map.d.f = 'file rec'
let g:which_key_map.d.g = 'grep'


" ======== Buffers ==========================
let g:which_key_map.b = { 'name' : '+buffers' }
let g:which_key_map.b.d = 'delete'
let g:which_key_map.b.o = 'only'

nnoremap <silent><leader>bd :bd<CR>
nnoremap <silent><leader>bo :only<CR>

" ======== highlighting  ====================
let g:which_key_map.h = { 'name' : '+highlight' }
let g:which_key_map.h.w = 'highlight word'
let g:which_key_map.h.c = 'clear word highlighting'
let g:which_key_map.h.n = 'clear word highlighting'

nnoremap <silent><leader>hw :call HighlightWordUnderCursor()<CR>
nnoremap <silent><leader>hc :match none<CR>
nnoremap <silent><leader>hn :noh<CR>

" ======== file group ======================
let g:which_key_map.f = { 'name' : '+file' }
let g:which_key_map.f.s = 'save-file'

" more convenient save
nnoremap <silent><leader>fs :w<CR>

" ========= vim-test =======================

let g:which_key_map.t = {
      \ 'name': '+test',
      \ 'f': 'file',
      \ 'l': 'last',
      \ 'n': 'nearest',
      \ 's': 'suite',
      \ 'v': 'visit',
      \ }

nnoremap <silent> <leader>tf  :TestFile<CR>
nnoremap <silent> <leader>tl  :TestLast<CR>
nnoremap <silent> <leader>tn  :TestNearest<CR>
nnoremap <silent> <leader>ts  :TestSuite<CR>
nnoremap <silent> <leader>tv  :TestVisit<CR>

" =========================================
let g:which_key_map.o = {
      \ 'name' : '+open',
      \ 'q' : 'quickfix'    ,
      \ 'l' : 'locationlist',
      \ 'u' : 'undotree'
      \ }

nnoremap <silent> <leader>oq  :Copen<CR>
nnoremap <silent> <leader>ol  :lopen<CR>
nnoremap <silent><leader>ou :UndotreeToggle<CR>

" ===============================================

let g:which_key_map.g = {
      \ 'name' : '+git',
      \ 'b' : 'blame'    ,
      \ 'l' : 'log',
      \ }

nnoremap <silent><leader>gb :Gblame<CR>
nnoremap <silent><leader>gl :Glog<CR>

" this allows to do :w!! for overwriting readonly files 
cnoremap w!! execute 'silent! write !sudo tee % >/dev/null' <bar> edit!

" ============ Vim which key ====================
nnoremap <silent> <leader> :<c-u>WhichKey ','<CR>

autocmd! FileType which_key
autocmd  FileType which_key set laststatus=0 noshowmode noruler
      \| autocmd BufLeave <buffer> set laststatus=2 showmode ruler

" Resize window {{{
noremap <M-Up> :res -2<CR>
noremap <M-Down> :res +2<CR>
noremap <M-Left> :vertical res -2<CR>
noremap <M-Right> :vertical res +2<CR>
" }}}

" }}}

" Colorscheme {{{
hi ALEError ctermfg=Red

highlight GitGutterAdd    ctermfg=green guifg=green
highlight GitGutterChange ctermfg=yellow guifg=yellow
highlight GitGutterDelete ctermfg=red guifg=red

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
augroup END

" }}}

" Markdown {{{
let g:markdown_fenced_languages = ['html', 'java', 'groovy', 'bash=sh', 
      \ 'sh', 'kotlin']
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

let g:undotree_SetFocusWhenToggle = 1
let g:undotree_ShortIndicators = 1
let g:undotree_WindowLayout = 2
" }}}

" Denite {{{

" Ripgrep {{{
if executable('rg')
  echom 'ripgrep'
  call denite#custom#var('grep', {
    \ 'command': ['rg'],
    \ 'default_opts': ['-i', '--vimgrep', '--no-heading', '-F'],
    \ 'recursive_opts': [],
    \ 'pattern_opt': [],
    \ 'separator': ['--'],
    \ 'final_opts': [],
    \ })
endif
" }}}

let s:menus = {}

call denite#custom#var('menu', 'menus', s:menus)

nnoremap <silent><leader>db :Denite buffer<CR>
nnoremap <silent><leader>df :Denite file/rec<CR>
nnoremap <silent><leader>dg :Denite grep:::!<CR>
nnoremap <silent><leader>dq :Denite -buffer-name=grep -default-action=quickfix grep:::!<CR>

nnoremap <silent><M-b> :Denite buffer<CR>
nnoremap <silent><M-p> :Denite file/rec<CR>
nnoremap <silent><space>g :Denite grep:::!<CR>
nnoremap <silent><M-g> :Denite -buffer-name=grep -default-action=quickfix grep:::!<CR>

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
  nnoremap <silent><buffer><expr> i
  \ denite#do_map('open_filter_buffer')
  nnoremap <silent><buffer><expr> s
  \ denite#do_map('toggle_select')
  nnoremap <silent><buffer><expr> a
  \ denite#do_map('toggle_select_all')
endfunction<Paste>


" }}}

set completeopt=noinsert,noselect,menuone 
set shortmess+=c


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

" Colors {{{
hi! MatchParen cterm=NONE,bold gui=NONE,bold guibg=green guifg=yellow
" }}}

" Projectionist {{{
let g:projectionist_heuristics = {
      \}
" }}}

" vim.test {{{

let test#strategy = 'dispatch_background'

" }}}

" rainbow_parentheses {{{

let g:rainbow#pairs = [['(', ')'], ['[', ']'], ['{', '}']]

augroup rainbow
  autocmd!
  autocmd FileType * RainbowParentheses
augroup END

" }}}

" Is at the end so that specializations can insert things too 
call which_key#register(',', "g:which_key_map")

" vim: foldmethod=marker foldlevel=0 foldenable
