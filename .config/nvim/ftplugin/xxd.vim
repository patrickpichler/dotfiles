" without the xxd command this is all pointless
if !executable('xxd')
	finish
endif

" don't insert a newline in the final line if it
" doesn't already exist, and don't insert linebreaks
setlocal binary noendofline
silent %!xxd -g 1
%s/\r$//e

" put the autocmds into a group for easy removal later
augroup ftplugin-xxd
	" erase any existing autocmds on buffer
	autocmd! * <buffer>

	" before writing, translate back to binary
	autocmd BufWritePre <buffer> let b:xxd_cursor = getpos('.')
	autocmd BufWritePre <buffer> silent %!xxd -r

	" after writing, restore hex view and mark unmodified
	autocmd BufWritePost <buffer> silent %!xxd -g 1
	autocmd BufWritePost <buffer> %s/\r$//e
	autocmd BufWritePost <buffer> setlocal nomodified
	autocmd BufWritePost <buffer> call setpos('.', b:xxd_cursor) | unlet b:xxd_cursor

	" update text column after changing hex values
	autocmd TextChanged,InsertLeave <buffer> let b:xxd_cursor = getpos('.')
	autocmd TextChanged,InsertLeave <buffer> silent %!xxd -r
	autocmd TextChanged,InsertLeave <buffer> silent %!xxd -g 1
	autocmd TextChanged,InsertLeave <buffer> call setpos('.', b:xxd_cursor) | unlet b:xxd_cursor
augroup END

" when filetype is set to no longer be "xxd," put the binary
" and endofline settings back to what they were before, remove
" the autocmds, and replace buffer with its binary value
let b:undo_ftplugin = 'setl bin< eol< | execute "au! ftplugin-xxd * <buffer>" | execute "silent %!xxd -r"'
