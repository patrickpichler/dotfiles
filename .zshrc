#load zgen
source "${HOME}/.zgen/zgen.zsh"

# if the init scipt doesn't exist
if ! zgen saved; then

  # specify plugins here
  zgen oh-my-zsh
  zgen oh-my-zsh plugins/git
  zgen oh-my-zsh plugins/sudo
  zgen oh-my-zsh plugins/command-not-found
  zgen oh-my-zsh plugins/asdf
  zgen oh-my-zsh plugins/scala
  zgen oh-my-zsh plugins/sbt

  zgen oh-my-zsh themes/robbyrussell

  zgen load zsh-users/zsh-syntax-highlighting

  zgen load spwhitt/nix-zsh-completions
  
  # generate the init script from plugins above
  zgen save
fi

. $HOME/.nix-profile/etc/profile.d/nix.sh

. $HOME/.asdf/asdf.sh
. $HOME/.asdf/completions/asdf.bash

if [ -s $HOME/development/scripts ]; then
  PATH=$PATH:~/development/scripts
fi

export NVIM_CONFIG_DIR="$HOME/.config/nvim"

alias edit_zsh_config="nvim ~/.zshrc"
alias edit_xmonad_config="nvim ~/.xmonad/xmonad.hs"
alias edit_nvim_config="nvim $NVIM_CONFIG_DIR"

if [ -n "$NVIM_LISTEN_ADDRESS" ]; then
  alias nvim='echo "No nesting!"'
fi

alias vim=nvim

alias config='/usr/bin/git --git-dir=$HOME/.myconf/ --work-tree=$HOME'

export EDITOR='/usr/local/bin/nvim'

