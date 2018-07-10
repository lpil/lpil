-module(algodub_infer_test).
-include_lib("eunit/include/eunit.hrl").

% id_test() ->
	% {"id", OK "forall[a] a -> a"},

% one_test() ->
	% {"one", OK "int"},

% x_test() ->
%   check("x", {error, {variable_not_found, "x"}}).

% x_in_y_test() ->
%   check("let x = x in y", {error, {variable_not_found, "x"}}).

% x_in_x_test() ->
%   {"let x = id in x", OK "forall[a] a -> a"},

id_fun_literal_test() ->
  check("fun y -> y", {ok, "'a -> 'a"}).

let_id_fun_literal_test() ->
  check("let x = fun y -> y in x", {ok, "'a -> 'a"}).

	% {"fun x -> x", OK "forall[int] int -> int"},
	% {"pair", OK "forall[a b] {a, b) -> pair[a, b]") ,
	% {"pair", OK "forall[z x] {x, z) -> pair[x, z]") ,

% fun_x_let_y_fun_z_test() ->
%   check("fun x -> let y = fun z -> z in y", {ok, "a -> b -> b"}).

	% {"let f = fun x -> x in let id = fun y -> y in eq{f, id)", OK "bool"},
	% {"let f = fun x -> x in let id = fun y -> y in eq_curry{f){id)", OK "bool"},
	% {"let f = fun x -> x in eq{f, succ)", OK "bool"},
	% {"let f = fun x -> x in eq_curry{f){succ)", OK "bool"},
	% {"let f = fun x -> x in pair{f{one}, f{true))", OK "pair[int, bool]"},
	% {"fun f -> pair{f{one}, f{true))", fail},
	% {"let f = fun x y -> let a = eq{x, y) in eq{x, y) in f", OK "forall[a] {a, a) -> bool"},
	% {"let f = fun x y -> let a = eq_curry{x){y) in eq_curry{x){y) in f",
	% 	OK "forall[a] {a, a) -> bool"},
	% {"id{id)", OK "forall[a] a -> a"},
	% {"choose{fun x y -> x, fun x y -> y)", OK "forall[a] {a, a) -> a"},
	% {"choose_curry{fun x y -> x){fun x y -> y)", OK "forall[a] {a, a) -> a"},
	% {"let x = id in let y = let z = x{id) in z in y", OK "forall[a] a -> a"},
	% {"cons{id, nil)", OK "forall[a] list[a -> a]"},
	% {"cons_curry{id){nil)", OK "forall[a] list[a -> a]"},
	% {"let lst1 = cons{id, nil) in let lst2 = cons{succ, lst1) in lst2", OK "list[int -> int]"},
	% {"cons_curry{id){cons_curry{succ){cons_curry{id){nil)))", OK "list[int -> int]"},
	% {"plus{one, true)", error "cannot unify types int and bool"},
	% {"plus{one)", error "unexpected number of arguments"},
	% {"fun x -> let y = x in y", OK "forall[a] a -> a"},
	% {"fun x -> let y = let z = x{fun x -> x) in z in y", OK "forall[a b] {{a -> a) -> b) -> b"},
	% {"fun x -> fun y -> let x = x{y) in x{y)", OK "forall[a b] {a -> a -> b) -> a -> b"},
	% {"fun x -> let y = fun z -> x{z) in y", OK "forall[a b] {a -> b) -> a -> b"},
	% {"fun x -> let y = fun z -> x in y", OK "forall[a b] a -> b -> a"},
	% {"fun x -> fun y -> let x = x{y) in fun x -> y{x)",
	% 	OK "forall[a b c] {{a -> b) -> c) -> {a -> b) -> a -> b"},
	% {"fun x -> let y = x in y{y)", error "recursive types"},
	% {"fun x -> let y = fun z -> z in y{y)", OK "forall[a b] a -> b -> b"},
	% {"fun x -> x{x)", error "recursive types"},
	% {"one{id)", error "expected a function"},
	% {"fun f -> let x = fun g y -> let _ = g{y) in eq{f, g) in x",
	% 	OK "forall[a b] {a -> b) -> {a -> b, a) -> bool"},
	% {"let const = fun x -> fun y -> x in const", OK "forall[a b] a -> b -> a"},
	% {"let apply = fun f x -> f{x) in apply", OK "forall[a b] {a -> b, a) -> b"},
	% {"let apply_curry = fun f -> fun x -> f{x) in apply_curry", OK "forall[a b] {a -> b) -> a -> b"}
	% ].

check(Source, Expected) ->
  {ok, Tokens, _} = algodub_tokenizer:string(Source),
  {ok, AST} = algodub_parser:parse(Tokens),
  case algodub_infer:infer(AST) of
    {ok, Type} ->
      ?assertEqual(Expected, {ok, algodub:type_to_string(Type)});

    Error ->
      ?assertEqual(Expected, Error)
  end.
