open Graphql;

type role =
  | User
  | Admin;

type user = {
  id: int,
  name: string,
  role
};

let users = [
  {id: 1, name: "Kai", role: Admin},
  {id: 2, name: "Bob", role: User},
  {id: 3, name: "Amy", role: User}
];

let role =
  Schema.(
    enum
      "role"
      doc::"The role of a user"
      values::[enum_value "USER" value::User, enum_value "ADMIN" value::Admin]
  );

let user =
  Schema.(
    obj
      "user"
      doc::"A user in the system"
      fields::(
        fun _ => [
          field
            "id"
            doc::"Unique user identifier"
            typ::(non_null int)
            args::Arg.empty
            resolve::(fun () p => p.id),
          field "name" typ::(non_null string) args::Arg.empty resolve::(fun () p => p.name),
          field "role" typ::(non_null role) args::Arg.empty resolve::(fun () p => p.role)
        ]
      )
  );

let schema =
  Schema.(
    schema [
      (field "users" typ: non_null (list (non_null user)))
        args::Arg.empty resolve::(fun () () => users)
    ]
  );
