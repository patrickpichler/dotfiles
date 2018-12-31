
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

c.editor.command = [ 'kitty', 'bash', '-c', 'eval $EDITOR {file}']

c.auto_save.session = True
c.auto_save.interval = 5000

with config.pattern('*://web.whatsapp.com/') as p:
    p.content.headers.user_agent = 'Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/58.0.3029.110 Safari/537.36'
