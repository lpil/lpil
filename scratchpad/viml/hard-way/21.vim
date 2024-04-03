""""""""""""""""""
"  Conditionals  "
""""""""""""""""""

" Multiline statements can be placed on a single line by using
" the | character as a delimeter.

echom "foo" | echom "bar"


" If statements

if 1
  echo "1 is truthy, so this will print"
endif

if 0
  echo "0 is falsy, so this will print"
endif

" Type coercion.
" VimL is *awful*, so it will coerce strings to ints.

echo "hello" + 10
" => 10

echo "10hello" + 10
" => 20

echo "hello10" + 10
" => 10

" If a string starts with a number it it coerced to that number.
" Otherwise it is coerced to 0.
"
" ...
"
" I KNOW, RIGHT?

if 0
  echo "Hey, look, there's elseif."
elseif 1
  echo "And else."
else
  echo "How handy."
endif
