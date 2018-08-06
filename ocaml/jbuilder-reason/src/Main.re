open Core;

let merge = (acc, x) => acc ++ sprintf("%d", x);

[1, 2, 3]
|> List.fold(~init="", ~f=merge)
|> printf("Printing a message: %s\n")
|> List.rev
