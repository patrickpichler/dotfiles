function __fish_print_installed_packages --description 'Print list of packages'
  xbps-query -l | string replace -r '^[^ ]* ([^ ]*)(-[^- ]*) .*$' '$1'
end

complete -c xbps-remove -f -a '(__fish_print_installed_packages)'

complete -c xbps-remove -r -F -s C -l config -d 'Specifies a path to the XBPS configuration directory.'

complete -c xbps-remove -r -F -s c -l cachedir -d 'Specifies a path to the cach directory, where binary packages are stored.'

complete -c xbps-remove -s d -l debug -d 'Enables extra debugging shown to stderr.'

complete -c xbps-remove -s F -l force-revdeps -d 'Forcefully remove package even if there are reverse dependencies and/or broken shared libraries (DANGEROUS!).'

complete -c xbps-remove -s f -l force -d 'Forcefully remove package files even if they have been modified.'

complete -c xbps-remove -s h -l help -d 'Show the help message.'

complete -c xbps-remove -s n -l dry-run -d 'Show what actions would be done but don\'t do anything.'

complete -c xbps-remove -s O -l clean-cache -d 'Cleans cache directory removing obsolete binary packages.'

complete -c xbps-remove -s o -l remove-orphans -d 'Removes installed package orphans that were installed automatically (as dependencies) and are not currently dependencies of any installed package.'

complete -c xbps-remove -s R -l recursive -d 'Recurisvely removes packages that were installed by PKGNAME and aren\'t required by other installed packages.'

complete -c xbps-remove -F -r -s r -l rootdir -d 'Specifies a ful path for the target root directory.'

complete -c xbps-remove -s v -l verbose -d 'Enables verbose messages.'

complete -c xbps-remove -s y -l yes -d 'Assume yes to all questions and avoid interactive questions.'

complete -c xbps-remove -s V -l version -d 'Show the version information.'
