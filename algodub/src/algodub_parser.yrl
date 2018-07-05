Nonterminals
grammar expr type type_forall simple_expr ident_list expr_comma_list
simple_type type_comma_list
.

Terminals

%%token <string> IDENT
%%token FUN LET IN FORALL
%%token LPAREN RPAREN LBRACKET RBRACKET
%%token ARROW EQUALS COMMA
%%token EOF

'fun' 'let' in forall '(' ')' '[' ']' '=' '->' ',' '\'' ident.

Rootsymbol grammar.

%%start expr_eof
%%type <Expr.expr> expr_eof
%%start ty_eof
%%type <Expr.ty> ty_eof
%%start ty_forall_eof
%%type <Expr.ty> ty_forall_eof

%expr_eof:
%	| expr EOF        { $1 }

%ty_eof:
%	| ty EOF          { $1 }

%ty_forall_eof:
%	| ty_forall EOF   { $1 }
%

grammar -> expr : '$1'.
grammar -> type : '$1'.
grammar -> type_forall : '$1'.

%expr:
%	| simple_expr                         { $1 }
%	| LET IDENT EQUALS expr IN expr       { Let($2, $4, $6) }
%	| FUN ident_list ARROW expr           { Fun($2, $4) }

expr -> simple_expr : '$1'.
expr -> 'let' ident '=' expr in expr : #ast_let{name = v('$2'), value = '$4', then = '$6'}.
expr -> 'fun' ident_list '->' expr : #ast_fun{args = '$2', body = '$4'}.

%simple_expr:
%	| IDENT                                             { Var $1 }
%	| LPAREN expr RPAREN                                { $2 }
%	| simple_expr LPAREN expr_comma_list RPAREN         { Call($1, $3) }
%	| simple_expr LPAREN RPAREN                         { Call($1, []) }

simple_expr -> ident : #ast_var{name = v('$1')}.
simple_expr -> '(' expr ')' : '$2'.
simple_expr -> simple_expr '(' expr_comma_list ')' : #ast_call{func = '$1', args = '$3'}.
simple_expr -> simple_expr '(' ')' : #ast_call{func = '1', args = []}.

%ident_list:
%	| IDENT               { [$1] }
%	| IDENT ident_list    { $1 :: $2 }

ident_list -> ident : ['$1'].
ident_list -> ident ident_list : ['$1'|'$2'].

%expr_comma_list:
%	| expr                          { [$1] }
%	| expr COMMA expr_comma_list    { $1 :: $3 }

expr_comma_list -> ident : ['$1'].
expr_comma_list -> ident ',' ident_list : ['$1'|'$3'].

%ty_forall:
%	| ty                                        { $1 }
%	| FORALL LBRACKET ident_list RBRACKET ty    { replace_ty_constants_with_vars $3 $5 }

type_forall -> forall '[' ident_list ']' type : replace_type_constants_with_vars('$3', '$5').

%ty:
%	| simple_ty                                         { $1 }
%	| LPAREN RPAREN ARROW ty                            { TArrow([], $4) }
%	| simple_ty ARROW ty                                { TArrow([$1], $3) }
%	| LPAREN ty COMMA ty_comma_list RPAREN ARROW ty     { TArrow($2 :: $4, $7) }

type -> simple_type : #type_const{name = v('$1')}.
type -> '(' ')' '->' type : #type_arrow{args = [], return = '$4'}.
type -> simple_type '->' type : #type_arrow{args = ['$1'], return = '$3'}.
type -> '(' type ',' type_comma_list ')' '->' type : #type_arrow{args = ['$1'], return = '$3'}.

%simple_ty:
%	| IDENT                                         { TConst $1 }
%	| simple_ty LBRACKET ty_comma_list RBRACKET     { TApp($1, $3) }
%	| LPAREN ty RPAREN                              { $2 }

simple_type -> '\'' ident : #type_const{name = '$2'}.
simple_type -> simple_type '[' type_comma_list ']' : #type_app{type = '$1', args = '$3'}.
simple_type -> '(' type ')' : '$2'.

%ty_comma_list:
%	| ty                        { [$1] }
%	| ty COMMA ty_comma_list    { $1 :: $3 }

type_comma_list -> type : ['$1'].
type_comma_list -> type ',' type_comma_list : ['$1'|'$3'].
Erlang code.

-include("algodub.hrl").

%%{

%open Expr
%open Infer

%let replace_ty_constants_with_vars var_name_list ty =
%	let env = List.fold_left
%		(fun env var_name -> Env.extend env var_name (new_gen_var ()))
%		Env.empty var_name_list
%	in
%	let rec f ty = match ty with
%		| TConst name -> begin
%				try
%					Env.lookup env name
%				with Not_found -> ty
%			end
%		| TVar _ -> ty
%		| TApp(ty, ty_arg_list) ->
%				TApp(f ty, List.map f ty_arg_list)
%		| TArrow(param_ty_list, return_ty) ->
%				TArrow(List.map f param_ty_list, f return_ty)
%	in
%	f ty

%%}

replace_type_constants_with_vars(VarNameList, Type) ->
  InsertVariable =
    fun(VarName, EnvAcc) ->
      Var = algodub_infer:new_gen_var(),
      algodub_infer:env_extend(EnvAcc, VarName, Var)
    end,
  Env = lists:foldl(InsertVariable, algodub_infer:env_empty(), VarNameList),
  LinkTypeVars = fun(F, Ty) ->
    case Ty of
      #type_const{name = Name} ->
        case algodub_infer:env_lookup(Env, Name) of
          {ok, Var} -> Var;
          error -> Ty
        end;

      #type_var{} ->
        Ty;

      #type_app{type = AppType, args = TypeArgList} ->
        #type_app{type = F(F, AppType),
                  args = lists:map(fun(T) -> F(F, T) end, TypeArgList)};

      #type_arrow{args = ParamTypeList, return = ReturnType} ->
        #type_arrow{args = lists:map(fun(T) -> F(F, T) end, ParamTypeList),
                    return = F(F, ReturnType)}
    end
  end,
  LinkTypeVars(LinkTypeVars, Type).


v({_, _, V}) ->
  V.
