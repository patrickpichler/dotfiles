if [ -d $HOME/bin ] ; then
  PATH=$HOME/bin:$PATH
fi

if [ -d $HOME/.local/bin ] ; then
  PATH=$HOME/.local/bin:$PATH
fi

if [ -s $HOME/development/scripts ]; then
  PATH=$PATH:~/development/scripts
fi

if [ -e $HOME/.asdf/shims ]; then
  PATH=$HOME/.asdf/shims:$PATH
fi

if [ -d $HOME/.cargo/bin ]; then
  PATH="$HOME/.cargo/bin:$PATH"
fi

if [ -e $HOME/.nix-profile/etc/profile.d/nix.sh ]; then
  . $HOME/.nix-profile/etc/profile.d/nix.sh;
elif [ -e /etc/profile.d/nix.sh ]; then
  . /etc/profile.d/nix.sh
fi 

export WINIT_HIDPI_FACTOR=1

export PATH
