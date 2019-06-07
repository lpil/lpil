open Graphql.Schema;

type role =
  | User
  | Admin;

type user = {
  id: int,
  name: string,
  role,
};

let users = [
  {id: 1, name: "Alice", role: Admin},
  {id: 2, name: "Bob", role: User},
];

let role: typ(unit, option(role)) =
  enum(
    "role",
    ~doc="The role of a user",
    ~values=[
      enum_value("USER", ~value=User),
      enum_value("ADMIN", ~value=Admin),
    ],
  );

let user = {
  let id =
    field(
      "id",
      ~doc="Unique user identifier",
      ~typ=non_null(int),
      ~args=Arg.Nil,
      ~resolve=(_ctx, p) =>
      p.id
    );

  let name =
    field("name", ~typ=non_null(string), ~args=Arg.Nil, ~resolve=(_ctx, p) =>
      p.name
    );

  let role =
    field("role", ~typ=non_null(role), ~args=Arg.Nil, ~resolve=(_ctx, p) =>
      p.role
    );

  obj("user", ~doc="A user in the system", ~fields=_ => [id, name, role]);
};

let schema =
  schema([
    field(
      "users",
      ~typ=non_null(list(non_null(user))),
      ~args=Arg.Nil,
      ~resolve=(_ctx, ()) =>
      users
    ),
  ]);
