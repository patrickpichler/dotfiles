function ssh --wraps 'ssh'
  TERM=xterm-256color command ssh $argv
end
