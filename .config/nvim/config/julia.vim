
let g:default_julia_version = '1.0'

" language server
let g:LanguageClient_autoStart = 1
let g:LanguageClient_serverCommands.julia = ['julia', '--startup-file=no', '--history-file=no', '-e', '
\       using LanguageServer;
\       server = LanguageServer.LanguageServerInstance(stdin, stdout, false);
\       server.runlinter = true;
\       run(server);
\   ']
