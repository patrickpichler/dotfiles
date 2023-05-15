function __fish_print_suggestions --description 'Print list of packages'
  xbps-query -Rs '' | string replace -r '^[^ ]* ([^ ]*)(-[^- ]*) .*$' '$1'
end

complete -c xbps-install -f -a '(__fish_print_suggestions)'

complete -c xbps-install -r -F -s C -l config -d 'Specifies a path to the XBPS configuration directory.'

complete -c xbps-install -r -F -s c -l cachedir -d 'Specifies a path to the cach directory, where binary packages are stored.'

complete -c xbps-install -s d -l debug -d 'Enables extra debugging shown to stderr.'

complete -c xbps-install -s D -l download-only -d 'Only download packages to the cache, do not do any other installation steps.'

complete -c xbps-install -s f -l force -d 'Force instalation of package, overwriting regular package files and symlinks, but preserving configuration files. If -f is specified twice, all files will be unpacked, even config files.'

complete -c xbps-install -s h -l help -d 'Show the help message.'

complete -c xbps-install -s I -l ignore-file-conflicts -d 'Ignore detected file conflicts in transaction.'

complete -c xbps-install -s i -l ignore-conf-repos -d 'Ignore repositories defined in configuration files.'

complete -c xbps-install -s M -l memory-sync -d 'For remote repositories, the data is fetched and stored in memory for the current operation. This ignores the existing on-disk repository archives in rootdir.'

complete -c xbps-install -s n -l dry-run -d 'Show what actions would be done but don\'t do anything.'

complete -c xbps-install -s R -l repository -r -d 'Appends the specified repository to the top of the list.'

complete -c xbps-install -l reproducible -d 'Enables reproducible mode in pkgdb.'

complete -c xbps-install -F -r -s r -l rootdir -d 'Specifies a ful path for the target root directory.'

complete -c xbps-install -s S -l sync -d 'Synchronize remote repository index files.'

complete -c xbps-install -s U -l unpack-only -d 'Packages to be installed or upgraded in the transaction won\'t be configured, just unpacked.'

complete -c xbps-install -s u -l update -d 'Performs a full system upgrade (except those on hold).'

complete -c xbps-install -s v -l verbose -d 'Enables verbose messages.'

complete -c xbps-install -s y -l yes -d 'Assume yes to all questions and avoid interactive questions.'

complete -c xbps-install -s V -l version -d 'Show the version information.'
