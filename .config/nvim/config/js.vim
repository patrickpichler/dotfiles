" ================================
" ========== Config ==============
" ================================

autocmd BufWritePost *.js AsyncRun -post=checktime ./node_modules/.bin/eslint --fix %

let g:LanguageClient_serverCommands = {
      \ 'javascript': ['javascript-typescript-langserver'],
      \ }

let g:gutentags_project_root = ['package.json']

" ================================
" ===== Additional Plugins =======
" ================================

packadd vim-jsx

" ================================


" ================================
" ==== Specialized keymap ========
" ================================
