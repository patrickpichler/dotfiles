
function config \
   --wraps 'git' \
   --description 'Git config command for dotfiles'

  git --git-dir=$HOME/.myconf --work-tree=$HOME $argv

end
