% type name = string

-type name() :: string().

% type expr =
% 	| Var of name                           (* variable *)
% 	| Call of expr * expr list              (* application *)
% 	| Fun of name list * expr               (* abstraction *)
% 	| Let of name * expr * expr             (* let *)

-record(var, {name :: name()}).
-record(call, {func :: expr(), args :: list(expr())}).
-record(func, {args :: list(name()), body :: expr()}).
-record(let_, {name :: name(), value :: expr(), then :: expr()}).

-type expr() :: #var{} | #call{} | #func{} | #let_{}.

% type id = int
% type level = int

-type id() :: integer().
-type level() :: integer().

-type type_var_reference() :: reference().

% type ty =
% 	| TConst of name                    (* type constant: `int` or `bool` *)
% 	| TApp of ty * ty list              (* type application: `list[int]` *)
% 	| TArrow of ty list * ty            (* function type: `(int, int) -> int` *)
% 	| TVar of tvar ref                  (* type variable *)

-record(type_const, {name :: name()}).
-record(type_app, {type :: type(), args :: list(type())}).
-record(type_arrow, {args :: list(type()), return :: type()}).
-record(type_var, {var :: type_var_reference()}).

-type type() :: #type_const{} | #type_app{} | #type_arrow{} | #type_var{}.

% and tvar =
% 	| Unbound of id * level
% 	| Link of ty
% 	| Generic of id

-record(tvar_unbound, {id :: id(), level :: level()}).
-record(tvar_link, {type :: type()}).
-record(tvar_generic, {id :: id()}).

-type tvar() :: #tvar_unbound{} | #tvar_link{} | #tvar_generic{}.
