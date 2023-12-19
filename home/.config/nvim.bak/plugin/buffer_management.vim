function! s:CycleHiddenBuffers(command)
  let num_buffers = winnr('$')
  let buffer_range = range(1, num_buffers)
  let visible_buffers = map(buffer_range, 'winbufnr(v:val)')

  for num in buffer_range
    if index(visible_buffers, winbufnr(winnr())) == -1
      break
    endif
    execute a:command
  endfor
endfunction

command! BN call s:CycleHiddenBuffers('bnext')
command! BP call s:CycleHiddenBuffers('bprevious')

nnoremap , :BN<CR>

" Close buffer, without closing the window/split
nnoremap d, :bp\|bd #<CR>
