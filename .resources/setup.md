# Dependencies:
* lux (https://github.com/Ventto/lux)
* nitrogen 
* alacritty (https://github.com/jwilm/alacritty)
* playerctl (https://github.com/acrisci/playerctl)
* xdotool 
* xvkbd 
* dunst
* libsound2-dev
* julia (nix-env -i julia)
  * https://github.com/JuliaEditorSupport/LanguageServer.jl
* neovim-remote 
* polybar (https://github.com/jaagr/polybar)
* rofi (https://github.com/DaveDavenport/rofi)
* pamixer
* numlockx

# Fonts
* ttf-material-design-icons-git
* ttf-font-awesome
* google-fonts-ttf

# Notes
## GPG
* Setup different pinentry variants
  * add `pinentry-program [home-path]/.local/bin/pinentry-env` to `~/.gnugpg/gpg-agent.conf`
* Increase password cache time
  * add `default-cache-ttl` and `max-cache-ttl` entries to ~/.gnugpg/gpg-agent.conf

## IntelliJ

### Focus issues

In order to make IntelliJ behave nicely with focus, we need to set
the `suppress.focus.stealing` flag, in the `idea.properties` file to
false. The easiest way to achieve this, is to open IntelliJ, press
`CTRL-A`, type `edit custom properties` and hit enter. This will open
the `idea.properties` file we need to edit. Now put there a new line
with `suppress.focus.stealing=false`. Now restart IntelliJ and you are
good to go.

## /etc/profile doesn't get sourced
In order to force sddm to sourcep `/etc/profile` when `zsh` is the 
login shell, the `Xsession` script has to be adapted. Simply put
`[ -f /etc/profile ] && emulate sh -c 'source /etc/profile'` in the
`*/zsh)` branch of the case. 

