
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

c.editor.command = [ "kitty", "bash", "-c", "eval $EDITOR {file}"]

c.auto_save.session = True
c.auto_save.interval = 5000
