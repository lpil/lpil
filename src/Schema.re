let typeDefs = [GraphQlQuery.typeDef, GraphQlEpisode.typeDef] |> List.fold_left (^) "";

let schema =
  GraphQLTools.makeExecutableSchema {
    "typeDefs": typeDefs,
    "resolvers": {"Query": GraphQlQuery.resolvers, "Episode": GraphQlEpisode.resolvers}
  };
