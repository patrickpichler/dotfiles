function theme
  argparse -n theme --min-args 1 --max-args 1 d/dark l/light -- $argv
  or return

  switch $argv[1]
case light
case dark
case '*'
    echo 'theme: only light/dark values are allowed'
    return 1
  end

  set color $argv[1]
  fish -c "set --universal theme_background $color"
end
