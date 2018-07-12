-module(algodub_infer).
-include("algodub.hrl").

-export([infer/1, infer/3, env_extend/3, env_lookup/2, env_empty/0,
         new_gen_var/0, get_tvar_ref/1]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

% let current_id = ref 0

% let next_id () =
% 	let id = !current_id in
% 	current_id := id + 1 ;
% 	id

% let reset_id () = current_id := 0

next_id() ->
  Id = get(current_infer_id),
  put(current_infer_id, Id + 1),
  Id.

% let new_var level = TVar (ref (Unbound(next_id (), level)))
% let new_gen_var () = TVar (ref (Generic(next_id ())))

new_tvar_ref(TVar) ->
  Id = get(current_tvar_ref_id),
  put(current_tvar_ref_id, Id + 1),
  put({tvar_ref, Id}, TVar),
  {tvar_ref, Id}.

put_tvar_ref({tvar_ref, Id}, TVar) ->
  put({tvar_ref, Id}, TVar),
  {tvar_ref, Id}.

get_tvar_ref({tvar_ref, Id}) ->
  get({tvar_ref, Id}).

new_var(Level) ->
  TVar = #tvar_unbound{id = next_id(), level = Level},
  #type_var{var = new_tvar_ref(TVar)}.

new_gen_var() ->
  TVar = #tvar_generic{id = next_id()},
  #type_var{var = new_tvar_ref(TVar)}.

% exception Error of string
% let error msg = raise (Error msg)

infer_error(Msg) ->
  error({algodub_infer_error, Msg}).

% module Env = struct
% 	module StringMap = Map.Make (String)
% 	type env = ty StringMap.t

% 	let empty : env = StringMap.empty
% 	let extend env name ty = StringMap.add name ty env
% 	let lookup env name = StringMap.find name env
% end

-type env() :: map().

env_empty() ->
  maps:new().

env_extend(Env, {ident, _, Name}, Type) ->
  env_extend(Env, Name, Type);
env_extend(Env, Name, Type) when is_list(Name) ->
  maps:put(Name, Type, Env).

env_lookup(Env, Name) ->
  maps:find(Name, Env).

% let occurs_check_adjust_levels tvar_id tvar_level ty =
% 	let rec f = function
% 		| TVar {contents = Link ty} -> f ty
% 		| TVar {contents = Generic _} -> assert false
% 		| TVar ({contents = Unbound(other_id, other_level)} as other_tvar) ->
% 				if other_id = tvar_id then
% 					error "recursive types"
% 				else
% 					if other_level > tvar_level then
% 						other_tvar := Unbound(other_id, tvar_level)
% 					else
% 						()
% 		| TApp(ty, ty_arg_list) ->
% 				f ty ;
% 				List.iter f ty_arg_list
% 		| TArrow(param_ty_list, return_ty) ->
% 				List.iter f param_ty_list ;
% 				f return_ty
% 		| TConst _ -> ()
% 	in
% 	f ty

occurs_check_adjust_levels(TVarId, TVarLevel, Type) ->
  F = fun
    (F, #type_var{var = TVarRef}) ->
      case get_tvar_ref(TVarRef) of
        #tvar_link{type = LinkedType} ->
          F(F, LinkedType);

        #tvar_generic{} ->
          error("occurs_check_adjust_levels assert false");

        #tvar_unbound{id = OtherId, level = OtherLevel} ->
          case OtherId =:= TVarId of
            true -> infer_error(recursive_types);
            false -> case OtherLevel > TVarLevel of
              true -> put_tvar_ref(TVarRef, #tvar_unbound{id = OtherId, level = TVarLevel});
              false -> ok
            end
          end
      end;

    (F, #type_app{type = AppType, args = TypeArgList}) ->
      F(F, AppType),
      lists:foreach(fun(T) -> F(F, T) end, TypeArgList);

    (F, #type_arrow{args = ParamTyList, return = ReturnTy}) ->
      lists:foreach(fun(T) -> F(F, T) end, ParamTyList),
      F(F, ReturnTy);

    (_, #type_const{}) ->
      ok
  end,
  F(F, Type).

-ifdef(TEST).
% thing_test() ->
%   TVar = #type_var{var = #tvar_link{type = 100}},
%   Result = occurs_check_adjust_levels(1, 1, TVar).
-endif.

% let rec unify ty1 ty2 =
% 	if ty1 == ty2 then () else
% 	match (ty1, ty2) with
% 		| TConst name1, TConst name2 when name1 = name2 -> ()
% 		| TApp(ty1, ty_arg_list1), TApp(ty2, ty_arg_list2) ->
% 				unify ty1 ty2 ;
% 				List.iter2 unify ty_arg_list1 ty_arg_list2
% 		| TArrow(param_ty_list1, return_ty1), TArrow(param_ty_list2, return_ty2) ->
% 				List.iter2 unify param_ty_list1 param_ty_list2 ;
% 				unify return_ty1 return_ty2
% 		| TVar {contents = Link ty1}, ty2 -> unify ty1 ty2
% 		| ty1, TVar {contents = Link ty2} -> unify ty1 ty2
%
% 		| TVar {contents = Unbound(id1, _)}, TVar {contents = Unbound(id2, _)} when id1 = id2 ->
% 				assert false (* There is only a single instance of a particular type variable. *)
%
% 		| TVar ({contents = Unbound(id, level)} as tvar), ty ->
% 				occurs_check_adjust_levels id level ty ;
% 				tvar := Link ty
% 		| ty, TVar ({contents = Unbound(id, level)} as tvar) ->
% 				occurs_check_adjust_levels id level ty ;
% 				tvar := Link ty
% 		| _, _ -> error ("cannot unify types " ^ string_of_ty ty1 ^ " and " ^ string_of_ty ty2)

unify(Ty1, Ty2) ->
  CheckTVar = fun(TVarRef, Ty) ->
    case get_tvar_ref(TVarRef) of
      #tvar_link{type = Type} -> unify(Type, Ty);

      #tvar_unbound{id = Id, level = Level} ->
        case Ty of
          #type_var{var = TVarRef2} ->
            case get_tvar_ref(TVarRef2) of
              #tvar_unbound{id = Id2} when Id =:= Id2 ->
                error("Should only be a single instance of a type variable");

              _ ->
                occurs_check_adjust_levels(Id, Level, Ty),
                put_tvar_ref(TVarRef, #tvar_link{type = Ty})
            end;

          _ ->
            occurs_check_adjust_levels(Id, Level, Ty),
            put_tvar_ref(TVarRef, #tvar_link{type = Ty})
        end
    end
  end,

  case {Ty1, Ty2} of
    {Same, Same} ->
      ok;

    {#type_const{name = Same}, #type_const{name = Same}} ->
      ok;

    {#type_app{type = Type1, args = TyArgList1},
     #type_app{type = Type2, args = TyArgList2}} ->
      unify(Type1, Type2),
      lists:foreach(fun({X, Y}) -> unify(X, Y) end,
                    lists:zip(TyArgList1, TyArgList2));

    {#type_arrow{args = ParamTyList1, return = ReturnTy1},
     #type_arrow{args = ParamTyList2, return = ReturnTy2}} ->
      lists:foreach(fun({X, Y}) -> unify(X, Y) end,
                    lists:zip(ParamTyList1, ParamTyList2)),
      unify(ReturnTy1, ReturnTy2);

    {#type_var{var = TVarRef}, Ty} ->
      CheckTVar(TVarRef, Ty);

    {Ty, #type_var{var = TVarRef}} ->
      CheckTVar(TVarRef, Ty);

    {_, _} ->
      infer_error({cannot_unify, Ty1, Ty2})
  end.

% let rec generalize level = function
% 	| TVar {contents = Unbound(id, other_level)} when other_level > level ->
% 			TVar (ref (Generic id))
% 	| TApp(ty, ty_arg_list) ->
% 			TApp(generalize level ty, List.map (generalize level) ty_arg_list)
% 	| TArrow(param_ty_list, return_ty) ->
% 			TArrow(List.map (generalize level) param_ty_list, generalize level return_ty)
% 	| TVar {contents = Link ty} -> generalize level ty
% 	| TVar {contents = Generic _} | TVar {contents = Unbound _} | TConst _ as ty -> ty

-spec generalize(non_neg_integer(), type()) -> type().
generalize(Level, Type) ->
  case Type of
    #type_var{var = TVarRef} ->
      case get_tvar_ref(TVarRef) of
        #tvar_unbound{id = Id, level = OtherLevel} when OtherLevel > Level ->
          NewTvarRef = new_tvar_ref(#tvar_generic{id = Id}),
          #type_var{var = NewTvarRef};

        #tvar_link{type = LinkedType} ->
          generalize(Level, LinkedType);

        _Other ->
          Type
      end;

    #type_app{type = ContainerType, args = ArgsTypes} ->
      #type_app{type = generalize(Level, ContainerType),
                args = lists:map(fun(T) -> generalize(Level, T) end, ArgsTypes)};

    #type_arrow{args = ArgsTypes, return = ReturnType} ->
      #type_arrow{return = generalize(Level, ReturnType),
                  args = lists:map(fun(T) -> generalize(Level, T) end, ArgsTypes)};

    #type_const{} ->
      Type
  end.

% let instantiate level ty =
% 	let id_var_map = Hashtbl.create 10 in
% 	let rec f ty = match ty with
% 		| TConst _ -> ty
% 		| TVar {contents = Link ty} -> f ty
% 		| TVar {contents = Generic id} -> begin
% 				try
% 					Hashtbl.find id_var_map id
% 				with Not_found ->
% 					let var = new_var level in
% 					Hashtbl.add id_var_map id var ;
% 					var
% 			end
% 		| TVar {contents = Unbound _} -> ty
% 		| TApp(ty, ty_arg_list) ->
% 				TApp(f ty, List.map f ty_arg_list)
% 		| TArrow(param_ty_list, return_ty) ->
% 				TArrow(List.map f param_ty_list, f return_ty)
% 	in
% 	f ty

thread_map(Fun, List, State) ->
  Reducer =
    fun(Item, {AccList, AccState}) ->
      {NewItem, NewState} = Fun(Item, AccState),
      {[NewItem|AccList], NewState}
    end,
  {MappedList, NewState} = lists:foldl(Reducer, {[], State}, List),
  {lists:reverse(MappedList), NewState}.


-spec instantiate(non_neg_integer(), type()) -> type().
instantiate(Level, Ty) ->
  F = fun
    (_, #type_const{} = Type, IdVarMap) ->
      {Type, IdVarMap};

    (F, #type_app{type = Type, args = TypeArgList}, IdVarMap) ->
      {NewTypeArgsList, IdVarMap2} = thread_map(F, TypeArgList, IdVarMap),
      {NewType, IdVarMap3} = F(F, Type, IdVarMap2),
      NewApp = #type_app{type = NewType, args = NewTypeArgsList},
      {NewApp, IdVarMap3};

    (F, #type_arrow{args = TypeArgList, return = Type}, IdVarMap) ->
      {NewTypeArgsList, IdVarMap2} = thread_map(fun(X, Acc) -> F(F, X, Acc) end,
                                                TypeArgList,
                                                IdVarMap),
      {NewType, IdVarMap3} = F(F, Type, IdVarMap2),
      NewArrow = #type_arrow{args = NewTypeArgsList, return = NewType},
      {NewArrow, IdVarMap3};

    (F, #type_var{var = TVarRef}, IdVarMap) ->
      case get_tvar_ref(TVarRef) of
        #tvar_link{type = Type} ->
          F(F, Type, IdVarMap);

        #tvar_generic{id = Id} ->
          case maps:find(Id, IdVarMap) of
            {ok, Type} ->
              {Type, IdVarMap};

            error ->
              Var = new_var(Level),
              {Var, maps:put(Id, Var, IdVarMap)}
          end;

        #tvar_unbound{} ->
          {Ty, IdVarMap}
      end
  end,
  {NewType, _NewIdVarMap} = F(F, Ty, maps:new()),
  NewType.

% let rec match_fun_ty num_params = function
% 	| TArrow(param_ty_list, return_ty) ->
% 			if List.length param_ty_list <> num_params then
% 				error "unexpected number of arguments"
% 			else
% 				param_ty_list, return_ty
% 	| TVar {contents = Link ty} -> match_fun_ty num_params ty
% 	| TVar ({contents = Unbound(id, level)} as tvar) ->
% 			let param_ty_list =
% 				let rec f = function
% 					| 0 -> []
% 					| n -> new_var level :: f (n - 1)
% 				in
% 				f num_params
% 			in
% 			let return_ty = new_var level in
% 			tvar := Link (TArrow(param_ty_list, return_ty)) ;
% 			param_ty_list, return_ty
% 	| _ -> error "expected a function"

match_fun_type(NumParams, Ty) ->
  case Ty of
    #type_arrow{args = ParamTyList, return = ReturnTy} ->
      case length(ParamTyList) =:= NumParams of
        false -> infer_error(unexpected_number_of_arguments);
        true -> {ParamTyList, ReturnTy}
      end;

    #type_var{var = TVarRef} ->
      case get_tvar_ref(TVarRef) of
        #tvar_link{type = Type} ->
          match_fun_type(NumParams, Type);

        #tvar_unbound{level = Level} ->
          ExpandArgs =
            fun
              (_, 0) -> [];
              (F, N) -> [new_var(Level)|F(F, N - 1)]
            end,
          ParamTyList = ExpandArgs(ExpandArgs, NumParams),
          ReturnTy = new_var(Level),
          FunType = #type_arrow{args = ParamTyList, return = ReturnTy},
          put_tvar_ref(TVarRef, #tvar_link{type = FunType}),
          {ParamTyList, ReturnTy};

        _OtherType ->
          infer_error(expected_a_function)
      end;

    _OtherType ->
      infer_error(expected_a_function)
  end.

% let rec infer env level = function
% 	| Var name -> begin
% 			try
% 				instantiate level (Env.lookup env name)
% 			with Not_found -> error ("variable " ^ name ^ " not found")
% 		end
% 	| Fun(param_list, body_expr) ->
% 			let param_ty_list = List.map (fun _ -> new_var level) param_list in
% 			let fn_env = List.fold_left2
% 				(fun env param_name param_ty -> Env.extend env param_name param_ty)
% 				env param_list param_ty_list
% 			in
% 			let return_ty = infer fn_env level body_expr in
% 			TArrow(param_ty_list, return_ty)
% 	| Let(var_name, value_expr, body_expr) ->
% 			let var_ty = infer env (level + 1) value_expr in
% 			let generalized_ty = generalize level var_ty in
% 			infer (Env.extend env var_name generalized_ty) level body_expr
% 	| Call(fn_expr, arg_list) ->
% 			let param_ty_list, return_ty =
% 				match_fun_ty (List.length arg_list) (infer env level fn_expr)
% 			in
% 			List.iter2
% 				(fun param_ty arg_expr -> unify param_ty (infer env level arg_expr))
% 				param_ty_list arg_list
% 			;
% 			return_ty

-spec infer(env(), non_neg_integer(), ast()) -> type().
infer(Env, Level, AstNode) ->
  % erlang:display(AstNode),
  case AstNode of
    #ast_var{name = Name} ->
      case maps:find(Name, Env) of
        {ok, Var} -> instantiate(Level, Var);
        error -> infer_error({variable_not_found, Name})
      end;

    #ast_fun{args = ParamList, body = BodyExpr} ->
      ParamTypeList = lists:map(fun(_) -> new_var(Level) end, ParamList),
      Insert =
        fun({ParamName, ParamType}, AccEnv) ->
          env_extend(AccEnv, ParamName, ParamType)
        end,
      FnEnv = lists:foldl(Insert, Env, lists:zip(ParamList, ParamTypeList)),
      ReturnType = infer(FnEnv, Level, BodyExpr),
      #type_arrow{args = ParamTypeList, return = ReturnType};

    #ast_let{name = VarName, value = ValueExpr, then = BodyExpr} ->
      VarType = infer(Env, Level + 1, ValueExpr),
      GeneralizedType = generalize(Level, VarType),
      NewEnv = env_extend(Env, VarName, GeneralizedType),
      infer(NewEnv, Level, BodyExpr);

    #ast_call{func = FnExpr, args = ArgList} ->
      {ParamTypeList, ReturnType} =
        match_fun_type(length(ArgList), infer(Env, Level, FnExpr)),
      Check =
        fun({ParamType, ArgExpr}) ->
          unify(ParamType, infer(Env, Level, ArgExpr))
        end,
      lists:foreach(Check, lists:zip(ParamTypeList, ArgList)),
      ReturnType
  end.

-spec infer(ast()) -> {ok, type()} | {error, any()}.
infer(AstNode) ->
  put(current_infer_id, 0),
  put(current_tvar_ref_id, 0),
  try infer(env_empty(), 1, AstNode) of
    Type ->
      {ok, generalize(-1, Type)}
  catch
    error:{algodub_infer_error, Error} -> {error, Error}
  end.
