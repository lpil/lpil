type enum_value 'a = {
  name: string,
  doc: string,
  value: 'a
}
and scalar_type 'b = {
  name: string,
  doc: string
}
and object_type 'a 'b = {
  name: string,
  doc: string
  /* TODO */
  /* fields: arg_list 'a 'b */
  /* TODO */
  /* coerce : Graphql_parser.const_value -> ('b, string) result; */
}
and enum_type 'b = {
  name: string,
  doc: string,
  values: list (enum_value 'b)
}
and regular_arg 'a 'b = {
  name: string,
  doc: string,
  type_: arg_type 'a 'b
}
and default_arg 'a 'b = {
  name: string,
  doc: string,
  type_: arg_type 'a (option 'b => 'a),
  default: 'b
}
and arg_type _ _ =
  | Scalar (scalar_type 'b) :arg_type 'a (option 'b => 'a)
  | Object (object_type 'a 'b) :arg_type 'c (option 'a => 'c)
  | Enum (enum_type 'b) :arg_type 'a (option 'b => 'a)
  | List (arg_type 'a ('b => 'a)) :arg_type 'a (option (list 'b) => 'a)
  | NonNullable (arg_type 'a (option 'b => 'a)) :arg_type 'a ('b => 'a)
and arg 'a 'b =
  | Arg (regular_arg 'a 'b) :arg 'a 'b
  | DefaultArg (default_arg 'a 'b) :arg 'a ('b => 'a)
and arg_list _ _ =
  | Nil :arg_list 'a 'a
  | Cons (arg 'b ('c => 'b)) (arg_list 'a 'b) :arg_list 'a ('c => 'b);

/*
  Argument constructors
 */
let arg definition :arg _ _ => Arg definition;

let default_arg definition :arg _ _ => DefaultArg definition;

/*
  Base type constructors
 */
let scalar definition :arg_type _ _ => Scalar definition;

let object_ definition :arg_type _ _ => Object definition;

let enum definition :arg_type _ _ => Enum definition;

/*
  Built-in argument types
 */
let int =
  Scalar {
    name: "Int",
    doc: ""
    /* coerce: */
    /*   fun */
    /*   | `Int n => Ok n */
    /*   | _ => Error "Invalid int" */
  };

let string =
  Scalar {
    name: "String",
    doc: ""
    /* coerce: */
    /*   fun */
    /*   | `String s => Ok s */
    /*   | _ => Error "Invalid string" */
  };

let float =
  Scalar {
    name: "Float",
    doc: ""
    /* coerce: */
    /*   fun */
    /*   | `Float f => Ok f */
    /*   | `Int n => Ok (float_of_int n) */
    /*   | _ => Error "Invalid float" */
  };

let bool =
  Scalar {
    name: "Boolean",
    doc: ""
    /* coerce: */
    /*   fun */
    /*   | `Bool b => Ok b */
    /*   | _ => Error "Invalid boolean" */
  };

let guid =
  Scalar {
    name: "ID",
    doc: ""
    /* coerce: */
    /*   fun */
    /*   | `String s => Ok s */
    /*   | `Int n => Ok (string_of_int n) */
    /*   | _ => Error "Invalid ID" */
  };

let non_null typ => NonNullable typ;

let list typ => List typ;
