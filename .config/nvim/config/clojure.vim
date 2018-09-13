" ================================
" ========== Config ==============
" ================================


let g:LanguageClient_serverCommands = {
    \ 'clojure': ['bash', '-c', 'clojure-lsp'],
    \ }

let g:LanguageClient_rootMarkers = {
    \ 'clojure': ['project.clj', 'build.boot', 'deps.edn']
    \}

let g:gutentags_project_root = ['project.clj']

" ================================
" ===== Additional Plugins =======
" ================================

"call minpac#add('ElmCast/elm-vim')

" ================================


" ================================
" ==== Specialized keymap ========
" ================================

function! Expand(exp) abort
    let l:result = expand(a:exp)
    return l:result ==# '' ? '' : "file://" . l:result
endfunction

nnoremap <silent> <leader>lrcc :call LanguageClient#workspace_executeCommand('cycle-coll', [Expand('%:p'), line('.') - 1, col('.') - 1])<CR>
nnoremap <silent> <leader>lrth :call LanguageClient#workspace_executeCommand('thread-first', [Expand('%:p'), line('.') - 1, col('.') - 1])<CR>
nnoremap <silent> <leader>lrtt :call LanguageClient#workspace_executeCommand('thread-last', [Expand('%:p'), line('.') - 1, col('.') - 1])<CR>
nnoremap <silent> <leader>lrtf :call LanguageClient#workspace_executeCommand('thread-first-all', [Expand('%:p'), line('.') - 1, col('.') - 1])<CR>
nnoremap <silent> <leader>lrtl :call LanguageClient#workspace_executeCommand('thread-last-all', [Expand('%:p'), line('.') - 1, col('.') - 1])<CR>
nnoremap <silent> <leader>lrml :call LanguageClient#workspace_executeCommand('move-to-let', [Expand('%:p'), line('.') - 1, col('.') - 1, input('Binding name: ')])<CR>
nnoremap <silent> <leader>lril :call LanguageClient#workspace_executeCommand('introduce-let', [Expand('%:p'), line('.') - 1, col('.') - 1, input('Binding name: ')])<CR>
nnoremap <silent> <leader>lrel :call LanguageClient#workspace_executeCommand('expand-let', [Expand('%:p'), line('.') - 1, col('.') - 1])<CR>
nnoremap <silent> <leader>lram :call LanguageClient#workspace_executeCommand('create-missing-libspec', [Expand('%:p'), line('.') - 1, col('.') - 1])<CR>
