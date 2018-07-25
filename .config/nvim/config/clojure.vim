" ================================
" ========== Config ==============
" ================================


let g:LanguageClient_serverCommands = {
    \ 'clojure': ['clojure-lspb'],
    \ }

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

nnoremap <silent> crcc :call LanguageClient#workspace_executeCommand('cycle-coll', [Expand('%:p'), line('.') - 1, col('.') - 1])<CR>
nnoremap <silent> crth :call LanguageClient#workspace_executeCommand('thread-first', [Expand('%:p'), line('.') - 1, col('.') - 1])<CR>
nnoremap <silent> crtt :call LanguageClient#workspace_executeCommand('thread-last', [Expand('%:p'), line('.') - 1, col('.') - 1])<CR>
nnoremap <silent> crtf :call LanguageClient#workspace_executeCommand('thread-first-all', [Expand('%:p'), line('.') - 1, col('.') - 1])<CR>
nnoremap <silent> crtl :call LanguageClient#workspace_executeCommand('thread-last-all', [Expand('%:p'), line('.') - 1, col('.') - 1])<CR>
nnoremap <silent> crml :call LanguageClient#workspace_executeCommand('move-to-let', [Expand('%:p'), line('.') - 1, col('.') - 1, input('Binding name: ')])<CR>
nnoremap <silent> cril :call LanguageClient#workspace_executeCommand('introduce-let', [Expand('%:p'), line('.') - 1, col('.') - 1, input('Binding name: ')])<CR>
nnoremap <silent> crel :call LanguageClient#workspace_executeCommand('expand-let', [Expand('%:p'), line('.') - 1, col('.') - 1])<CR>
nnoremap <silent> cram :call LanguageClient#workspace_executeCommand('create-missing-libspec', [Expand('%:p'), line('.') - 1, col('.') - 1])<CR>
