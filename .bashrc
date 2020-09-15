# .bashrc

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

if [[ $(ps --no-header --pid=$PPID --format=cmd) != "fish" && -z "$BASH_EXECUTION_STRING" ]]
then
  SHELL=fish
  exec fish
fi

