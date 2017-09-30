type t = {
  name: string,
  number: int
};

let hope = {name: "A New Hope", number: 4};

let empire = {name: "The Empire Strikes Back", number: 5};

let jedi = {name: "The Return of the Jedi", number: 6};

let getName e => e.name;

let getNumber e => e.number;

let typeDef = "type Episode { name: String!, number: Int! }";

let resolvers = {"name": getName, "number": getNumber};
