# xbps-reconfigure

function __fish_print_installed_packages --description 'Print list of packages'
  xbps-query -l | string replace -r '^[^ ]* ([^ ]*)(-[^- ]*) .*$' '$1'
end

complete -c xbps-reconfigure -f -a '(__fish_print_installed_packages)'
complete -c xbps-reconfigure -s a -l all --description 'Configures all packages.'
complete -c xbps-reconfigure -s C -l config -r -F --description 'Specifies a path to the XBPS configuration directory.'
complete -c xbps-reconfigure -s d -l debug --description 'Enables extra debugging shown to stderr.'
complete -c xbps-reconfigure -s f -l force --description 'Forcefully reconfigure package even if it was configured previously.'
complete -c xbps-reconfigure -s h -l help --description 'Show the help message.'
complete -c xbps-reconfigure -s i -l ignore -r -a '(__fish_print_installed_packages)' --description 'Ignore PKG when configuring all packages with a, Fl -all.'
complete -c xbps-reconfigure -s r -l rootdir -F -r --description 'Specifies a path for the target root directory.'
complete -c xbps-reconfigure -s v -l verbose --description 'Enables verbose messages.'
complete -c xbps-reconfigure -s V -l version --description 'Show the version information.  El FILES -tag -width /var/db/xbps/.'

