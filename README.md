# Setup

Just execute 
```
git clone --separate-git-dir=$HOME/.myconf https://github.com/patrickpichler/dotfiles $HOME/myconf-tmp

cp ~/myconf-tmp/.gitmodules ~  # If you use Git submodules

rm -r ~/myconf-tmp/

alias config='/usr/bin/git --git-dir=$HOME/.myconf/ --work-tree=$HOME'

# To hide untracked files
config config status.showUntrackedFiles no
```

# Taffybar

To make taffybar work:
```
cd ~/.config/taffybar && ln -s ~/.xmonad/taffybar-git taffybar
```


# Dependencies

Easiest to install with nix

```
nix-env -i neovim
nix-env -i fd
```
