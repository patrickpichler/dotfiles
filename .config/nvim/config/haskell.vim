" ================================
" ========== Config ==============
" ================================


let g:LanguageClient_serverCommands = {
    \ 'haskell': ['hie', '--lsp'],
    \ }

let g:ale_linters = {
  \ 'haskell' : ['hlint', 'stack-ghc', 'stack-build'], 
  \ }

" ================================
" ===== Additional Plugins =======
" ================================

"call minpac#add('ElmCast/elm-vim')
call minpac#add('neovimhaskell/haskell-vim')

" ================================


" ================================
" ==== Specialized keymap ========
" ================================


