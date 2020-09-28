
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

Plug 'honza/vim-snippets'

Plug 'AndrewRadev/bufferize.vim'
Plug 'tommcdo/vim-exchange'
Plug 'junegunn/vim-peekaboo'

Plug 'axelf4/vim-strip-trailing-whitespace'

Plug 'Shougo/denite.nvim', { 'do': ':UpdateRemotePlugins', 'tag': '*' }

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

" ============= LSP ===========================
Plug 'neoclide/coc.nvim', {'branch': 'release'}

" ============= Coc extensions ===============
Plug 'neoclide/coc-emmet', {'tag': '*', 'do': 'yarn install --frozen-lockfile'}
Plug 'neoclide/coc-java', {'tag': '*', 'do': 'yarn install --frozen-lockfile'}
Plug 'neoclide/coc-json', {'tag': '*', 'do': 'yarn install --frozen-lockfile'}
Plug 'neoclide/coc-tsserver', {'tag': '*', 'do': 'yarn install --frozen-lockfile'}
Plug 'neoclide/coc-tslint-plugin', {'tag': '*', 'do': 'yarn install --frozen-lockfile'}
Plug 'neoclide/coc-html', {'tag': '*', 'do': 'yarn install --frozen-lockfile'}
Plug 'neoclide/coc-snippets', {'tag': '*', 'do': 'yarn install --frozen-lockfile'}
Plug 'neoclide/coc-python', {'tag': '*', 'do': 'yarn install --frozen-lockfile'}

" ============= New text objects ==============
Plug 'kana/vim-textobj-user'
Plug 'vim-utils/vim-line'
Plug 'wellle/targets.vim'
Plug 'kana/vim-textobj-entire'

" ============= Clojure ==========================
Plug 'Olical/conjure', { 'for': 'clojure', 'tag': '*' }
Plug 'guns/vim-sexp', { 'for': 'clojure' }
Plug 'tpope/vim-sexp-mappings-for-regular-people', { 'for': 'clojure' }

" ============ PlantUML ==========================
Plug 'aklt/plantuml-syntax'
Plug 'tyru/open-browser.vim'
Plug 'weirongxu/plantuml-previewer.vim'

" ===========================================

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

" }}}

" Lightline {{{
let g:lightline = {
      \ 'colorscheme': 'jellybeans',
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
  nnoremap <silent><buffer><expr> i
  \ denite#do_map('open_filter_buffer')
  nnoremap <silent><buffer><expr> s
  \ denite#do_map('toggle_select')
  nnoremap <silent><buffer><expr> a
  \ denite#do_map('toggle_select_all')
endfunction

" }}}

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

hi! MatchParen cterm=NONE,bold gui=NONE,bold guibg=green guifg=yellow
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

" vim: foldmethod=marker foldlevel=0 foldenable
