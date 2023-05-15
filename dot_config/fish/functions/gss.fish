function gss \
   --wraps 'git status --short'\
   --description 'shortcut for `git status --short`'
  git status --short $argv
end
