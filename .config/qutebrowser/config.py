
import dracula.draw

dracula.draw.blood(c, {
    'spacing': {
        'vertical': 6,
        'horizontal': 8
    },
    'font': {
        'family': 'Menlo, Terminus, Monaco, Monospace',
        'size': 10
    }
})

c.editor.command = [ 'kitty', 'nvim', '-f', '{file}', '-c', 'normal {line}G{column0}l']

c.auto_save.session = True
c.auto_save.interval = 5000

c.aliases = {
        "w": "session-save",
        "q": "quit",
        "wq": "quit --save",
        "b": "buffer",
        "view-in-mpv": "spawn --userscript view-in-mpv",
        "readability": "spawn --userscript readability"
        }

c.content.autoplay = False

config.bind('<Ctrl-r>', 'open-editor')
config.bind('<Ctrl-r>', 'open-editor', mode='insert')
config.bind('<Ctrl-l>', 'edit-url')
config.bind(',v', 'edit-url')

config.bind('<Ctrl-n>', 'completion-item-focus --history next', mode='command')
config.bind('<Ctrl-p>', 'completion-item-focus --history prev', mode='command')

c.url.searchengines = {
    'DEFAULT': 'https://search.privacytools.io/?language=en-US&q={}',
    '!sp': 'https://startpage.com/do/search?q={}',
    '!ddg': 'https://duckduckgo.com/?q={}',
    '!gh': 'https://github.com/search?q={}',
    '!osm': 'https://www.openstreetmap.org/search?query={}',
    '!reddit': 'https://www.reddit.com/search/?q={}',
    '!clojure': 'https://clojuredocs.org/search?q={}',
    '!dictcc': 'https://www.dict.cc/?s={}',
    '!steam': 'https://store.steampowered.com/search/?term={}',
    '!so': 'https://stackoverflow.com/search?q={}',
    '!amazon': 'https://www.amazon.de/s?k={}',
    '!yt': 'https://www.youtube.com/results?search_query={}',
    '!devdocs': 'https://devdocs.io#q={}',
    '!git': 'https://git-scm.com/search/results?search={}',
    '!geizhals': 'https://geizhals.at/?fs={}',
    '!wiki': 'https://en.wikipedia.org/w/index.php?search={}',
}

for i in range(1, 50):
    c.aliases[f"b{i}"] = f"buffer {i}"

with config.pattern('*://web.whatsapp.com/') as p:
    p.content.headers.user_agent = 'Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/58.0.3029.110 Safari/537.36'

with config.pattern('https://meet.jit.si/') as p:
    p.content.media_capture = True
