---
title: Lexing and parsing with Leex and Yecc
tags:
  - Erlang
  - Elixir
  - BEAM
  - Compilers
  - Lisp
---

I love working in Elixir, and the more Elixir I write the more I realise that
while the language itself is extremely well designed, it's the Erlang Virtual
machine (the BEAM), and the concurrent application building libraries (OTP)
that really get me excited. Wouldn't it be great to write a language for this
platform, so that we can understand it a little better? I think there's
potentially a lot of fun and learning to be had here, so I'm going to try and
do just that.

The first step of language compiling is to transform a string of source code
into an abstract syntax tree, which is a data structure containing the
syntactic meaning of the source code. Wikipedia has a [good article][wiki-ast]
on the subject if you want to read more.

[wiki-ast]: https://en.wikipedia.org/wiki/Abstract_syntax_tree

Erlang (and thus Elixir as well) includes two modules named Leex and Yecc that
are useful for this step. Leex is a lexer generator, it helps us break down a
source file into tiny fragments called tokens, and Yecc is a parser generator,
which helps us combine these tokens into an abstract syntax tree.

Even with tools like these parsing can get reasonably hairy, so my language is
going to be a simple Lisp language as the lightweight and perfectly regular
syntax is easy to parse.


Say we wanted to tokenize the Lisp code "`(add 1 (add 2 3))`", after some work
with Leex we might end up with a list of tokens that looks something like
this:

```elixir
[
  "(":  "(",
  atom: "add",
  int:  "1",
  "(":  "(",
  atom: "add",
  int:  "2",
  int:  "3",
  ")":  ")",
  ")":  ")",
]
```

Our string can be broken down into a combination of tokens of types `(`, `)`,
`atom,` or `int` all of which can have their own value.

Once we have these tokens we can use Yecc to build the abstract syntax tree,
which could look like this in Elixir:

```elixir
[:add, 1, [:add, 2, 3]]
```

Notice how similar this Elixir representation of the AST is to the actual Lisp
syntax?

```clojure
(add 1 (add 2 3))
```

This is why some say that Lisp has no syntax, you're more or less directly
writing the AST. It's also partly why it's so easy to parse!


## Setting up the project

Let's get to work. From here on out I'm going to be writing Erlang code, but
there's no reason you can't do all the same in Elixir. Except the code in your
Leex and Yecc modules that is, they have their own little Erlang-like DSLs.

Create up a new rebar project:

```console
$ rebar3 new lib lisp
===> Writing lisp/src/lisp.erl
===> Writing lisp/src/lisp.app.src
===> Writing lisp/rebar.config
===> Writing lisp/.gitignore
===> Writing lisp/LICENSE
===> Writing lisp/README.md
$ cd lisp
```

I want to keep my tests in another directory, so I edit my `rebar.config` for
this, create the directory, and then make a placeholder test which we can run
to check that everything works..

```erlang
%%% rebar.config
{erl_opts, [debug_info]}.
{eunit_tests, [{dir, "test"}]}. % This line is new.
{deps, []}.
```
```console
$ mkdir test
```
```erlang
% test/lisp_test.erl
-module(lisp_test).
-include_lib("eunit/include/eunit.hrl").

suite_test() ->
  ?assert(true).
```
```console
$ rebar3 eunit
===> Verifying dependencies...
===> Compiling lisp
===> Performing EUnit tests...
.

Finished in 0.010 seconds
1 tests, 0 failures
```

That works, so let's move onto the lexer.


## Tokenization with Leex

When we use Leex and Yecc we're effectively metaprogramming Erlang. We write a
description of our syntax in a declarative fashion, and they each generate a
regular Erlang module that will do the compilation for us. If our Leex file is
located at `src/lisp_lexer.xrl` we'll get a generated file at
`src/lisp_lexer.erl`. If you're using git it's best to ignore these generated
files.

```console
$ echo src/lisp_lexer.erl >> .gitignore
$ echo src/lisp_parser.erl >> .gitignore
```

Now to write some basic assertions about what we expect from our tokenizer.

```erlang
% test/lisp_lexer_test.erl
-module(lisp_lexer_test).
-include_lib("eunit/include/eunit.hrl").

-define(assertTokens(Code, Tokens),
        ?assertEqual(Tokens, lisp_lexer:string!(Code))).

int_test() ->
  ?assertTokens("1",    [{int, "1"}]),
  ?assertTokens("0007", [{int, "0007"}]),
  ?assertTokens("-23",  [{int, "-23"}]).

float_test() ->
  ?assertTokens("1.1",   [{float, "1.1"}]),
  ?assertTokens("29.12", [{float, "29.12"}]),
  ?assertTokens("-1.1",  [{float, "-1.1"}]).

string_test() ->
  ?assertTokens("\"Hi\"",     [{string, "Hi"}]),
  ?assertTokens("\"\"",       [{string, ""}]),
  ?assertTokens("\" \\\\ \"", [{string, " \\ "}]),
  ?assertTokens("\" \\\" \"", [{string, " \" "}]).

atom_test() ->
  ?assertTokens("add",       [{atom, "add"}]),
  ?assertTokens("minus_two", [{atom, "minus_two"}]),
  ?assertTokens("Bang!",     [{atom, "Bang!"}]),
  ?assertTokens("+",         [{atom, "+"}]),
  ?assertTokens("add-one",   [{atom, "add-one"}]).

expression_test() ->
  ?assertTokens("(add 1 (mult 2 3))",
                [{'(',  "("},
                 {atom, "add"},
                 {int,  "1"},
                 {'(',  "("},
                 {atom, "mult"},
                 {int,  "2"},
                 {int,  "3"},
                 {')',  ")"},
                 {')',  ")"}]).
```

`int_test/0` and `float_test/0` assert that the lexer can tokenize integers,
floats, numbers with preceeding 0s, and negative numbers.

`string_test/0` asserts that it can tokenize strings, empty strings, and
strings containing escaped quotes and slashes.

`atom_test/0` asserts that atoms containing various characters can be
tokenized correctly.

Lastly there is `expression_test/0`, which checks the result of the example
expression used earlier.

Time to write our lexer.

## Writing Leex

In Leex we use regular expressions to define a set of rules dictating what the
lexer should consider tokens. Each rule has an associated data structure or
either `skip_token` or `{token, YourToken}` which dictates the contents of the
token tuple to be returned, if any.

```erlang
{Regex} : DataStructure
```

Here's a Leex module that will make the tests pass:

```erlang
%%% src/lisp_lexer.xrl

Definitions. %% Regexes to be used in Rules

WS     = [\n\s\r\t]
Int    = [+-]?[0-9]+
Float  = [+-]?[0-9]+\.[0-9]+
String = "([^\\""]|\\.)*"
Atom   = [a-zA-Z_^!+=*\-\?]+

Rules. %% Mappings between regexes and token tuples

{WS}     : skip_token.
[(]      : {token, {'(' TokenChars}}.
[)]      : {token, {')' TokenChars}}.
{Int}    : {token, {int, TokenChars}}.
{Float}  : {token, {float, TokenChars}}.
{String} : {token, {string, strValue(TokenChars)}}.
{Atom}   : {token, {atom, TokenChars}}.


Erlang code. %% Helper functions

-export(['string!'/1]).

'string!'(Code) ->
 {ok, Tokens, _} = string(Code),
 Tokens.

strValue(S) when is_list(S) ->
  Contents  = tl(lists:droplast(S)),
  deescape(Contents, []).

deescape([$\\, C|Tail], Acc) -> deescape(Tail, [C|Acc]);
deescape([C|Tail]     , Acc) -> deescape(Tail, [C|Acc]);
deescape([]           , Acc) -> lists:reverse(Acc).
```

In the definitions section I've named a series of regexes that will match our
various token types.

In the Rules section I've specified token tuples for all the definitions
except `WS`, which we discard as our syntax doesn't care about whitespace.

For the value of most token tuples I use the `TokenChars` variable which
contains the characters matched by the regex. The string token is different,
for that one I drop the quotes and remove any escape characters to get the
writer's intended value.


## Parsing with Yecc

The next step is to transform the tokens into the AST, which is where Yecc
comes in. I'm going to write some tests again to make assertions about the
tree I expect. As the parser is so tightly coupled to the lexer I'm going to
test them both, rather than trying to isolate just the parser.

```erlang
-module(lisp_parser_test).
-include_lib("eunit/include/eunit.hrl").

-define(assertAST(Code, AST),
        ?assertEqual(AST, lisp_parser:'string!'(Code))).

literal_test() ->
  ?assertAST("",       []),
  ?assertAST("12345",  [12345]),
  ?assertAST("-5",     [-5]),
  ?assertAST("1.5",    [1.5]),
  ?assertAST("\"Hi\"", ["Hi"]),
  ?assertAST("sup",    [sup]),
  ?assertAST("()",     [[]]).

expression_test() ->
  ?assertAST("(mult 1 2)",    [[mult, 1, 2]]),
  ?assertAST("(+ 1 2 3)",     [['+', 1, 2, 3]]),
  ?assertAST("(+ 3 (- 5 1))", [['+', 3, ['-', 5, 1]]]).

multiple_expression_test() ->
  ?assertAST("(print hi) (id 2)", [[print, hi], [id, 2]]).
```

With these tests in place I can start creating the parser with Yecc.

Much like a Leec module, a Yeec module is made up of a fixed number of
sections.

### Terminals

A list of bottom level tokens that the parser can understand. In our case this
would be `'('`, `')'`, `int`, `float`, `string`, and `atom.

```erlang
Terminals int float string atom '(' ')'.
```

### Nonterminals

Higher level components that are made by composing terminals or other
nonterminals. For example, a nonterminal called `literal` could be an `atom`,
`int`, `float`, or a `string`.

```erlang
literal -> atom
literal -> int
literal -> float
literal -> string
```

A nonterminal called `elements` could be one or more literals, though this
would be expressed recursively, so `elements` is a `literal`, or a `literal`
followed by `elements`.

```erlang
elements -> element
elements -> element elements
```
