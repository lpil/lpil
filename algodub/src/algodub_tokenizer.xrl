Definitions.


% let ident = ['_' 'A'-'Z' 'a'-'z'] ['_' 'A'-'Z' 'a'-'z' '0'-'9']*
% let integer = ['0'-'9']+

Ident = [_A-Za-z][_A-Za-z0-9]*
% Integer = [0-9]+

Rules.

% rule token = parse
% 	| [' ' '\t' '\r' '\n']  { token lexbuf }
% 	| "fun"                 { FUN }
% 	| "let"                 { LET }
% 	| "in"                  { IN }
% 	| "forall"              { FORALL }
% 	| ident                 { IDENT (Lexing.lexeme lexbuf) }
% 	| '('     { LPAREN }
% 	| ')'     { RPAREN }
% 	| '['     { LBRACKET }
% 	| ']'     { RBRACKET }
% 	| '='     { EQUALS }
% 	| "->"    { ARROW }
% 	| ','     { COMMA }
% 	| eof     { EOF }
% 	| _       { raise Error }



[\s\t\r\n] : skip_token.
fun        : {token, {'fun', TokenLine}}.
let        : {token, {'let', TokenLine}}.
in         : {token, {in, TokenLine}}.
forall     : {token, {forall, TokenLine}}.
'\('       : {token, {'(', TokenChars}}.
'\)'       : {token, {')', TokenChars}}.
'\['       : {token, {'[', TokenChars}}.
'\]'       : {token, {']', TokenChars}}.
'='        : {token, {'=', TokenChars}}.
'->'       : {token, {'->', TokenChars}}.
','        : {token, {',', TokenChars}}.
'\''       : {token, {'\'', TokenChars}}.
{Ident}    : {token, {ident, TokenChars}}.
{WS}       : skip_token.


Erlang code.
