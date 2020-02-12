# ~/.profile: executed by the command interpreter for login shells.
# This file is not read by bash(1), if ~/.bash_profile or ~/.bash_login
# exists.
# see /usr/share/doc/bash/examples/startup-files for examples.
# the files are located in the bash-doc package.

# the default umask is set in /etc/profile; for setting the umask
# for ssh logins, install and configure the libpam-umask package.
#umask 022

# if running bash
if [ -n "$BASH_VERSION" ]; then
    # include .bashrc if it exists
    if [ -f "$HOME/.bashrc" ]; then
	. "$HOME/.bashrc"
    fi
fi

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

export $PATH
