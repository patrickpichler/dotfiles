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
  zgen oh-my-zsh plugins/docker
  zgen oh-my-zsh plugins/docker-compose
  zgen oh-my-zsh plugins/lein

  zgen oh-my-zsh themes/robbyrussell

  zgen load andrewferrier/fzf-z
  zgen load zsh-users/zsh-syntax-highlighting
  zgen load hlissner/zsh-autopair
  zgen load gko/ssh-connect
  zgen load voronkovich/gitignore.plugin.zsh

  zgen load spwhitt/nix-zsh-completions
  zgen load arzzen/calc.plugin.zsh
  
  # generate the init script from plugins above
  zgen save
fi

autoload -Uz compinit && compinit -i

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

if [[ $TERM == xterm-termite ]]; then
  . /etc/profile.d/vte.sh
  __vte_osc7
fi

# =====================
# ===== ALIAS =========
# =====================


# ==== Vim modes ======
alias vim=nvim
alias elmvim='export vim_mode=elm;nvim'
alias clojurevim='export vim_mode=clojure;nvim'
alias haskellvim='export vim_mode=haskell;nvim'

# =====================

alias config='git --git-dir=$HOME/.myconf/ --work-tree=$HOME'

# =====================

export EDITOR='nvim'

if [ -f ~/.extensions.zsh ]; then
  source ~/.extensions.zsh
fi 

prompt_nix_shell_setup
