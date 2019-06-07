(*
 * Originally written by Andreas Garnaes, adapted by Louis Pilfold to compile
 * with Bucklescript, which uses an older version of the OCaml compiler (and
 * thus has no inline records, boo).
 *
 * https://github.com/andreas/ocaml-graphql-server
 * https://andreas.github.io/2017/11/29/type-safe-graphql-with-ocaml-part-1/
 * https://andreas.github.io/2018/01/05/modeling-graphql-type-modifiers-with-gadts/
 *)

open Graphql_result

module Option = struct
  let map x ~f = match x with None -> None | Some y -> Some (f y)
end

(* IO *)
module type IO = sig
  type +'a t

  val return : 'a -> 'a t

  val bind : 'a t -> ('a -> 'b t) -> 'b t
end

(* Schema *)
module Make (Io : IO) = struct
  type +'a io = 'a Io.t

  module Io = struct
    include Io

    let ok x = Io.return (Ok x)
  end

  type deprecated = NotDeprecated | Deprecated of string option

  type 'a enum_value =
    {name: string; doc: string option; deprecated: deprecated; value: 'a}

  let enum_value ?doc ?(deprecated= NotDeprecated) name ~value =
    {name; doc; deprecated; value}

  let id: 'a. 'a -> 'a = fun x -> x

  module Arg = struct
    type _ arg_typ =
      (* | Scalar (name, doc, coerce) *)
      | Scalar:
          string * string option * (Js.Json.t -> ('a, string) result)
          -> 'a option arg_typ
      (* | Object (name, doc, fields, coerce) *)
      | Object:
          string * string option * ('a, 'b) arg_list * 'b
          -> 'a option arg_typ
      (* | Enum (name, doc, values) *)
      | Enum: string * string option * 'a enum_value list -> 'a option arg_typ
      | List: 'a arg_typ -> 'a list option arg_typ
      | NonNullable: 'a option arg_typ -> 'a arg_typ

    and _ arg =
      (* | Arg (name, doc, typ) *)
      | Arg: string * string option * 'a arg_typ -> 'a arg
      (* | DefaultArg (name, doc, typ, default) *)
      | DefaultArg: string * string option * 'a option arg_typ * 'a -> 'a arg

    and (_, _) arg_list =
      (* | [] : ('a, 'a) arg_list *)
      | Nil : ('a, 'a) arg_list
      (* | (::) : 'a arg * ('b, 'c) arg_list -> ('b, 'a -> 'c) arg_list *)
      | Cons: 'a arg * ('b, 'c) arg_list -> ('b, 'a -> 'c) arg_list

    let arg ?doc name ~typ = Arg (name, doc, typ)

    let arg' ?doc name ~typ ~default = DefaultArg (name, doc, typ, default)

    let scalar ?doc name ~coerce = Scalar (name, doc, coerce)

    let enum ?doc name ~values = Enum (name, doc, values)

    let obj ?doc name ~fields ~coerce = Object (name, doc, fields, coerce)

    (* Built-in argument types *)
    let int =
      (* TODO: real coerce *)
      let coerce n =
        let _ = ignore n in
        Ok 0
      in
      Scalar ("Int", None, coerce)

    let string =
      (* TODO: real coerce *)
      let coerce n =
        let _ = ignore n in
        Ok ""
      in
      Scalar ("String", None, coerce)

    let float =
      (* TODO: real coerce *)
      let coerce n =
        let _ = ignore n in
        Ok 0.
      in
      Scalar ("Float", None, coerce)

    let bool =
      (* TODO: real coerce *)
      let coerce n =
        let _ = ignore n in
        Ok true
      in
      Scalar ("Boolean", None, coerce)

    let guid =
      (* coerce = function *)
      (* | `String s -> Ok s *)
      (* | `Int n -> Ok (string_of_int n) *)
      (* | _ -> Error "Invalid ID" *)
      (* TODO: real coerce *)
      let coerce n =
        let _ = ignore n in
        Ok ""
      in
      Scalar ("ID", None, coerce)

    let non_null typ = NonNullable typ

    let list typ = List typ
  end

  (* Schema data types *)
  type 'a scalar = {name: string; doc: string option; coerce: 'a -> Js.Json.t}

  type 'a enum = {name: string; doc: string option; values: 'a enum_value list}

  type ('ctx, 'src) obj =
    {name: string; doc: string option; fields: ('ctx, 'src) field list Lazy.t}

  and (_, _) field =
    (* | Field (name, doc, deprecated, typ, args, resolve, lift) *)
    | Field:
        string
        * string option
        * deprecated
        * ('ctx, 'out) typ
        * ('a, 'args) Arg.arg_list
        * ('ctx -> 'src -> 'args)
        * ('a -> ('out, string) result Io.t)
        -> ('ctx, 'src) field

  and (_, _) typ =
    | Object: ('ctx, 'src) obj -> ('ctx, 'src option) typ
    | List: ('ctx, 'src) typ -> ('ctx, 'src list option) typ
    | NonNullable: ('ctx, 'src option) typ -> ('ctx, 'src) typ
    | Scalar: 'src scalar -> ('ctx, 'src option) typ
    | Enum: 'src enum -> ('ctx, 'src option) typ

  type 'ctx schema =
    {query: ('ctx, unit) obj; mutation: ('ctx, unit) obj option}

  let schema ?(mutation_name= "mutation") ?mutations ?(query_name= "query")
      fields =
    { query= {name= query_name; doc= None; fields= lazy fields}
    ; mutation=
        Option.map mutations ~f:(fun fields ->
            {name= mutation_name; doc= None; fields= lazy fields} ) }

  (* Constructor functions *)
  let obj ?doc name ~fields =
    let rec o = Object {name; doc; fields= lazy (fields o)} in
    o

  let field ?doc ?(deprecated= NotDeprecated) name ~typ ~args ~resolve =
    Field (name, doc, deprecated, typ, args, resolve, Io.ok)

  let io_field ?doc ?(deprecated= NotDeprecated) name ~typ ~args ~resolve =
    Field (name, doc, deprecated, typ, args, resolve, id)

  let enum ?doc name ~values = Enum {name; doc; values}

  let scalar ?doc name ~coerce = Scalar {name; doc; coerce}

  let list typ = List typ

  let non_null typ = NonNullable typ

  (* Built-in scalars *)
  let int: 'ctx. ('ctx, int option) typ =
    Scalar
      { name= "Int"
      ; doc= None
      ; coerce= (fun i -> i |> float_of_int |> Js.Json.number) }

  let string: 'ctx. ('ctx, string option) typ =
    Scalar {name= "String"; doc= None; coerce= Js.Json.string}

  let bool: 'ctx. ('ctx, bool option) typ =
    Scalar {name= "Boolean"; doc= None; coerce= Js.Json.boolean}

  let float: 'ctx. ('ctx, float option) typ =
    Scalar {name= "Float"; doc= None; coerce= Js.Json.number}

  let guid: 'ctx. ('ctx, string option) typ =
    Scalar {name= "ID"; doc= None; coerce= Js.Json.string}
end
