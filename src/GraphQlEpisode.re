/*
   GraphQL interface for Episodes
 */
let typeDef = {|

type Episode {
  name: String!
  number: Int!
}

|};

let getName (e: Episode.t) => e.name;

let getNumber (e: Episode.t) => e.number;

let resolvers = {"name": getName, "number": getNumber};
