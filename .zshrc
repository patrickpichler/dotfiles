#load zgen
source "${HOME}/.zgen/zgen.zsh"

# if the init scipt doesn't exist
if ! zgen saved; then

  # specify plugins here
  zgen oh-my-zsh
  zgen oh-my-zsh plugins/git
  zgen oh-my-zsh plugins/sudo
  zgen oh-my-zsh plugins/asdf
  zgen oh-my-zsh plugins/docker
  zgen oh-my-zsh plugins/docker-compose
  zgen oh-my-zsh plugins/lein
  zgen oh-my-zsh plugins/mvn
  zgen oh-my-zsh plugins/vi-mode
  zgen oh-my-zsh plugins/fd

  zgen oh-my-zsh themes/philips

  zgen load zsh-users/zsh-syntax-highlighting
  zgen load hlissner/zsh-autopair
  zgen load voronkovich/gitignore.plugin.zsh

  zgen load spwhitt/nix-zsh-completions
  
  # generate the init script from plugins above
  zgen save
fi

# ======== Varios autocompletion =========================

fpath=($HOME/.zsh/completions $fpath)

# ==========================================================

# ==== Gradle autocompletion ==============================

fpath=($HOME/.zsh/gradle-completion $fpath)

# =========================================================

autoload -Uz compinit && compinit -u

if [ -e $HOME/.asdf ]; then
  . $HOME/.asdf/asdf.sh
  . $HOME/.asdf/completions/asdf.bash
fi

if [ -e /usr/share/doc/fzf ]; then
  . /usr/share/doc/fzf/completion.zsh
  . /usr/share/doc/fzf/key-bindings.zsh
fi

# Custom style for syntax higlighting
typeset -A ZSH_HIGHLIGHT_STYLES

ZSH_HIGHLIGHT_STYLES[comment]='fg=cyan'

# ===================================
export NVIM_CONFIG_DIR="$HOME/.config/nvim"

# ==== Vim modes ======

function _start_vim(){
  export vim_mode=$1

  shift

  nvim $@

  unset vim_mode
}

function elmvim(){
  _start_vim "elm" $@
}
function clojurevim(){
  _start_vim "clojure" $@
}
function haskellvim(){
  _start_vim "haskell" $@
}
function jsvim(){
  _start_vim "js" $@
}
function juliavim(){
  _start_vim "julia" $@
}
function rustvim(){
  _start_vim "rust" $@
}

alias vim=nvim

# ===== FZF ===========
# Use fd (https://github.com/sharkdp/fd) instead of the default find
# command for listing path candidates.
# - The first argument to the function ($1) is the base path to start traversal
# - See the source code (completion.{bash,zsh}) for the details.
_fzf_compgen_path() {
  fd --hidden --follow --exclude ".git" . "$1"
}

# Use fd to generate the list for directory completion
_fzf_compgen_dir() {
  fd --type d --hidden --follow --exclude ".git" . "$1"
}

# =====================
# ===== ALIAS =========
# =====================

if [ -n "$NVIM_LISTEN_ADDRESS" ]; then
  alias nvim='echo "No nesting!"'
fi

alias config='git --git-dir=$HOME/.myconf/ --work-tree=$HOME'

alias edit-zsh-config="nvim ~/.zshrc"
alias edit-nvim-config="pushd . > /dev/null; cd $NVIM_CONFIG_DIR; nvim $NVIM_CONFIG_DIR; popd > /dev/null;"

# =====================


if type nvr > /dev/null ; then
  export EDITOR='nvr --remote -s'
else
  export EDITOR=nvim
fi

export KEYTIMEOUT=20

export MANPATH=:~/.local/share/man

# ==========================
# ====== Key bindings ======
# ==========================

autoload -U up-line-or-beginning-search
zle -N up-line-or-beginning-search

autoload -U down-line-or-beginning-search
zle -N down-line-or-beginning-search

# allow ctrl-p, ctrl-n for navigate history (standard behaviour)
bindkey '^P' up-line-or-beginning-search
bindkey '^N' down-line-or-beginning-search

bindkey '^X^E' edit-command-line

if [[ "${terminfo[kcbt]}" != "" ]]; then
  bindkey "${terminfo[kcbt]}" reverse-menu-complete   # [Shift-Tab] - move through the completion menu backwards
fi

# start typing + [Up-Arrow] - fuzzy find history forward
if [[ "${terminfo[kcuu1]}" != "" ]]; then
  bindkey "${terminfo[kcuu1]}" up-line-or-beginning-search
fi
# start typing + [Down-Arrow] - fuzzy find history backward
if [[ "${terminfo[kcud1]}" != "" ]]; then
  bindkey "${terminfo[kcud1]}" down-line-or-beginning-search
fi

[[ -n "$terminfo[khome]" ]] && bindkey "$terminfo[khome]" beginning-of-line
[[ -n "$terminfo[kend]" ]] && bindkey "$terminfo[kend]" end-of-line
[[ -n "$terminfo[kdch1]" ]] && bindkey "$terminfo[kdch1]" delete-char

bindkey '^[[1;5C' forward-word  # [Ctrl-RightArrow] - move forward one word
bindkey '^[[1;5D' backward-word # [Ctrl-LeftArrow] - move backward one word

bindkey -M vicmd 'v' visual-mode
bindkey -M vicmd '^v' edit-command-line

bindkey "^]^]" sudo-command-line

if [ -f ~/.extensions.zsh ]; then
  source ~/.extensions.zsh
fi 

export PATH="$HOME/.yarn/bin:$HOME/.config/yarn/global/node_modules/.bin:$PATH"
