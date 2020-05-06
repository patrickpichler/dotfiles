" ================================
" ========== Config ==============
" ================================

autocmd BufWritePost *.js AsyncRun -post=checktime ./node_modules/.bin/eslint --fix %

" ================================
" ==== Specialized keymap ========
" ================================

