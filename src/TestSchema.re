open GraphQl;

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
    enum {
      name: "role",
      doc: "The role of the user",
      values: [
        {name: "USER", doc: "Normal permissions", value: User},
        {name: "Admin", doc: "Super permissions", value: Admin}
      ]
    }
  );

let user =
  Schema.(
    object_ {
      name: "user",
      doc: "A user in the system"
      /* fields: fun _ => [ */
      /*   field { */
      /*     name: "id", */
      /*     doc: "Unique user identifier", */
      /*     type_: non_null int, */
      /*     args: Arg.empty, */
      /*     resolve: fun () p => p.id */
      /*   }, */
      /*   field { */
      /*     name: "name", */
      /*     doc: "Name of the user", */
      /*     type_: non_null string, */
      /*     args: Arg.empty, */
      /*     resolve: fun () p => p.name */
      /*   }, */
      /*   field { */
      /*     name: "role", */
      /*     doc: "Role of the user", */
      /*     type_: non_null role, */
      /*     args: Arg.empty, */
      /*     resolve: fun () p => p.role */
      /*   } */
      /* ] */
    }
  );

let schema =
  Schema.(
    schema [
      field {
        name: users,
        type_: non_null (list (non_null user)),
        args: Arg.empty,
        resolve: fun () () => users
      }
    ]
  );
