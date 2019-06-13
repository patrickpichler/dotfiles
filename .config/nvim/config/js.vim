" ================================
" ========== Config ==============
" ================================

autocmd BufWritePost *.js AsyncRun -post=checktime ./node_modules/.bin/eslint --fix %

let g:gutentags_project_root = ['package.json']

" ================================
" ==== Specialized keymap ========
" ================================

