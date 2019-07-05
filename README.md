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

For further information, checkout the ` ~/.resources/setup.md` file.
