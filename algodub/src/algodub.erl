-module(algodub).

-include("algodub.hrl").

% -export([type_to_string/1]).

% let string_of_ty ty : string =
% 	let id_name_map = Hashtbl.create 10 in
% 	let count = ref 0 in
% 	let next_name () =
% 		let i = !count in
% 		incr count ;
% 		let name = String.make 1 (Char.chr (97 + i mod 26)) ^
% 			if i >= 26 then string_of_int (i / 26) else ""
% 		in
% 		name
% 	in
% 	let rec f is_simple = function
% 		| TConst name -> name
% 		| TApp(ty, ty_arg_list) ->
% 				f true ty ^ "[" ^ String.concat ", " (List.map (f false) ty_arg_list) ^ "]"
% 		| TArrow(param_ty_list, return_ty) ->
% 				let arrow_ty_str = match param_ty_list with
% 					| [param_ty] ->
% 							let param_ty_str = f true param_ty in
% 							let return_ty_str = f false return_ty in
% 							param_ty_str ^ " -> " ^ return_ty_str
% 					| _ ->
% 							let param_ty_list_str = String.concat ", " (List.map (f false) param_ty_list) in
% 							let return_ty_str = f false return_ty in
% 							"(" ^ param_ty_list_str ^ ") -> " ^ return_ty_str
% 				in
% 				if is_simple then "(" ^ arrow_ty_str ^ ")" else arrow_ty_str
% 		| TVar {contents = Generic id} -> begin
% 					try
% 						Hashtbl.find id_name_map id
% 					with Not_found ->
% 						let name = next_name () in
% 						Hashtbl.add id_name_map id name ;
% 						name
% 				end
% 		| TVar {contents = Unbound(id, _)} -> "_" ^ string_of_int id
% 		| TVar {contents = Link ty} -> f is_simple ty
% 	in
% 	let ty_str = f false ty in
% 	if !count > 0 then
% 		let var_names = Hashtbl.fold (fun _ value acc -> value :: acc) id_name_map [] in
% 		"forall[" ^ String.concat " " (List.sort String.compare var_names) ^ "] " ^ ty_str
% 	else
% 		ty_str

% type_to_string(Type) ->
