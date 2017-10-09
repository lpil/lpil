external bodyParserJson : unit => Express.Middleware.t = "json" [@@bs.module "body-parser"];

let () = {
  let port = 8080;
  let app = Express.App.make ();
  let graphql_path = "/graphql";
  let schema =
    GraphQLTools.makeExecutableSchema {
      "typeDefs": Schema.typeDefs,
      "resolvers": {"Query": GraphQlQuery.resolvers}
    };
  let graphqlMiddleware = ApolloServerExpress.createGraphQLExpressMiddleware schema;
  let graphiqlMiddleware = ApolloServerExpress.createGraphiQLExpressMiddleware graphql_path;
  Express.App.use app (bodyParserJson ());
  Express.App.useOnPath app graphqlMiddleware path::graphql_path;
  Express.App.useOnPath app graphiqlMiddleware path::"/";
  Express.App.listen app ::port;
  Js.log ("Listening on http://localhost:" ^ string_of_int port)
};
