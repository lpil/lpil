let typeDefs = [GraphQlQuery.typeDef, GraphQlEpisode.typeDef] |> List.fold_left (^) "";

let schema = GraphQL.Utilities.buildSchema typeDefs;

let runQuery query => GraphQL.run schema rootValue::GraphQlQuery.resolvers query;
