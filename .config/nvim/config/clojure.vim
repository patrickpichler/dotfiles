" ================================
" ========== Config ==============
" ================================

let g:gutentags_project_root = ['project.clj', 'build.boot']


" ================================
" ==== Specialized keymap ========
" ================================

function! Expand(exp) abort
    let l:result = expand(a:exp)
    return l:result ==# '' ? '' : "file://" . l:result
endfunction

nnoremap <silent> <leader>lrcc :call CocRequest('clojure-lsp', 'workspace/executeCommand', {'command': 'cycle-coll', 'arguments': [Expand('%:p'), line('.') - 1, col('.') - 1]})<CR>
nnoremap <silent> <leader>lrcn :call CocRequest('clojure-lsp', 'workspace/executeCommand', {'command': 'clean-ns', 'arguments': [Expand('%:p'), line('.') - 1, col('.') - 1]})<CR>
nnoremap <silent> <leader>lrth :call CocRequest('clojure-lsp', 'workspace/executeCommand', {'command': 'thread-first', 'arguments': [Expand('%:p'), line('.') - 1, col('.') - 1]})<CR>
nnoremap <silent> <leader>lrtt :call CocRequest('clojure-lsp', 'workspace/executeCommand', {'command': 'thread-last', 'arguments': [Expand('%:p'), line('.') - 1, col('.') - 1]})<CR>
nnoremap <silent> <leader>lrtf :call CocRequest('clojure-lsp', 'workspace/executeCommand', {'command': 'thread-first-all', 'arguments': [Expand('%:p'), line('.') - 1, col('.') - 1]})<CR>
nnoremap <silent> <leader>lrtl :call CocRequest('clojure-lsp', 'workspace/executeCommand', {'command': 'thread-last-all', 'arguments': [Expand('%:p'), line('.') - 1, col('.') - 1]})<CR>
nnoremap <silent> <leader>lruw :call CocRequest('clojure-lsp', 'workspace/executeCommand', {'command': 'unwind-thread', 'arguments': [Expand('%:p'), line('.') - 1, col('.') - 1]})<CR>
nnoremap <silent> <leader>lrua :call CocRequest('clojure-lsp', 'workspace/executeCommand', {'command': 'unwind-all', 'arguments': [Expand('%:p'), line('.') - 1, col('.') - 1]})<CR>
nnoremap <silent> <leader>lrml :call CocRequest('clojure-lsp', 'workspace/executeCommand', {'command': 'move-to-let', 'arguments': [Expand('%:p'), line('.') - 1, col('.') - 1, input('Binding name: ')]})<CR>
nnoremap <silent> <leader>lril :call CocRequest('clojure-lsp', 'workspace/executeCommand', {'command': 'introduce-let', 'arguments': [Expand('%:p'), line('.') - 1, col('.') - 1, input('Binding name: ')]})<CR>
nnoremap <silent> <leader>lrel :call CocRequest('clojure-lsp', 'workspace/executeCommand', {'command': 'expand-let', 'arguments': [Expand('%:p'), line('.') - 1, col('.') - 1]})<CR>
nnoremap <silent> <leader>lref :call CocRequest('clojure-lsp', 'workspace/executeCommand', {'command': 'extract-function', 'arguments': [Expand('%:p'), line('.') - 1, col('.') - 1, input('Function name: ')]})<CR>
nnoremap <silent> <leader>lram :call CocRequest('clojure-lsp', 'workspace/executeCommand', {'command': 'add-missing-libspec', 'arguments': [Expand('%:p'), line('.') - 1, col('.') - 1]})<CR>

nnoremap <F9> :Dispatch lein compile<CR>
nnoremap <F8> :Dispatch lein test<CR>

let g:which_key_map.l = { 'name' : '+lsp' }
let g:which_key_map.l.r = { 'name': '+refactor' }
let g:which_key_map.l.r.c = { 'name': '+c' }
let g:which_key_map.l.r.c.c = 'cycle-coll'
let g:which_key_map.l.r.c.n = 'clean-ns'
let g:which_key_map.l.r.t = { 'name': '+thread' }
let g:which_key_map.l.r.t.h = 'thread-first'
let g:which_key_map.l.r.t.t = 'thread-last'
let g:which_key_map.l.r.t.f = 'thread-first-all'
let g:which_key_map.l.r.t.l = 'thread-last-all'
let g:which_key_map.l.r.u = { 'name': '+unwind' }
let g:which_key_map.l.r.u.w = 'unwind-thread'
let g:which_key_map.l.r.u.a = 'unwind-all'
let g:which_key_map.l.r.m = { 'name': '+move' }
let g:which_key_map.l.r.m.l = 'move-to-let'
let g:which_key_map.l.r.i = { 'name': '+introduce' }
let g:which_key_map.l.r.i.l = 'introduce-let'
let g:which_key_map.l.r.e = { 'name': '+e' }
let g:which_key_map.l.r.e.l = 'expand-let'
let g:which_key_map.l.r.e.f = 'extract-function'
let g:which_key_map.l.r.a = { 'name': '+add' }
let g:which_key_map.l.r.a.m = 'add-missing-libspec'

autocmd BufNewFile,BufRead *.boot set filetype=clojure

autocmd BufReadCmd,FileReadCmd,SourceCmd jar:file://* call s:LoadClojureContent(expand("<amatch>"))
 function! s:LoadClojureContent(uri)
  setfiletype clojure
  let content = CocRequest('clojure-lsp', 'clojure/dependencyContents', {'uri': a:uri})
  call setline(1, split(content, "\n"))
  setl nomodified
  setl readonly
endfunction
