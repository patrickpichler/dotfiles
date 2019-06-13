" ================================
" ========== Config ==============
" ================================

let g:LanguageClient_serverCommands.clojure = ['bash', '-c', 'clojure-lsp']

let g:LanguageClient_rootMarkers.clojure = ['project.clj', 'build.boot', 'deps.edn']

let g:gutentags_project_root = ['project.clj', 'build.boot']


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
nnoremap <silent> <leader>lrif :call LanguageClient#workspace_executeCommand('extract-function', [Expand('%:p'), line('.') - 1, col('.') - 1, input('Function name: ')])<CR>
nnoremap <silent> <leader>lrel :call LanguageClient#workspace_executeCommand('expand-let', [Expand('%:p'), line('.') - 1, col('.') - 1])<CR>
nnoremap <silent> <leader>lram :call LanguageClient#workspace_executeCommand('add-missing-libspec', [Expand('%:p'), line('.') - 1, col('.') - 1])<CR>


nnoremap <F9> :Dispatch lein compile<CR>
nnoremap <F8> :Dispatch lein test<CR>

let g:which_key_map.l = { 'name' : '+lsp' }
let g:which_key_map.l.r = { 'name': '+refactor' }
let g:which_key_map.l.r.c = { 'name': '+cycle' }
let g:which_key_map.l.r.c.c = 'cycle-coll'
let g:which_key_map.l.r.t = { 'name': '+thread' }
let g:which_key_map.l.r.t.h = 'thread-first'
let g:which_key_map.l.r.t.t = 'thread-last'
let g:which_key_map.l.r.t.f = 'thread-first-all'
let g:which_key_map.l.r.t.l = 'thread-last-all'
let g:which_key_map.l.r.m = { 'name': '+move' }
let g:which_key_map.l.r.m.l = 'move-to-let'
let g:which_key_map.l.r.i = { 'name': '+introduce' }
let g:which_key_map.l.r.i.l = 'introduce-let'
let g:which_key_map.l.r.i.f = 'introduce-function'
let g:which_key_map.l.r.e = { 'name': '+expand' }
let g:which_key_map.l.r.e.l = 'expand-let'
let g:which_key_map.l.r.a = { 'name': '+add' }
let g:which_key_map.l.r.a.m = 'add-missing-libspec'

autocmd BufNewFile,BufRead *.boot set filetype=clojure

