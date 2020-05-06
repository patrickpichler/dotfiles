
function __start_vim
  vim_mode=$argv[1] nvim $argv[2..-1]
end

function clojurevim
  __start_vim 'clojure' $argv
end

function haskellvim
  __start_vim 'haskell' $argv
end

function jsvim
  __start_vim 'js' $argv
end

function juliavim
  __start_vim 'julia' $argv
end

function rustvim
  __start_vim 'rust' $argv
end
