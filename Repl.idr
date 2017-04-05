module Main

main : IO ()
main =
  repl "What's up?\n" (\x => x ++ "? Cool.\n")
