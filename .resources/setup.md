# Dependencies:
* lux (https://github.com/Ventto/lux)
* nitrogen 
* kitty (https://github.com/kovidgoyal/kitty) 
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


# Notes

## IntelliJ

### Focus issues

In order to make IntelliJ behave nicely with focus, we need to set
the `suppress.focus.stealing` flag, in the `idea.properties` file to
false. The easiest way to achieve this, is to open IntelliJ, press
`CTRL-A`, type `edit custom properties` and hit enter. This will open
the `idea.properties` file we need to edit. Now put there a new line
with `suppress.focus.stealing=false`. Now restart IntelliJ and you are
good to go.
