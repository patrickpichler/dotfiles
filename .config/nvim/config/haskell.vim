" ================================
" ========== Config ==============
" ================================


let g:LanguageClient_serverCommands.'haskell'= ['hie', '--lsp']

let g:ale_linters = {
  \ 'haskell' : ['hlint', 'stack-ghc', 'stack-build'], 
  \ }


" ================================
" ==== Specialized keymap ========
" ================================


