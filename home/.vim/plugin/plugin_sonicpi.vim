let g:sonicpi_command = 'sonic-pi-pipe'

function s:MapSonicPiKeys()
  nnoremap <buffer> sd :SonicPiSendBuffer<CR>
  nnoremap <buffer> SS :SonicPiStop<CR>
endfunction
autocmd Filetype ruby call s:MapSonicPiKeys()
