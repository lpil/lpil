let typeDefs = Query.typeDef ^ " " ^ Episode.typeDef;

let resolvers = {"Query": Query.resolvers, "Episode": Episode.resolvers};

let schema = GraphQLTools.makeExecutableSchema {"typeDefs": typeDefs, "resolvers": resolvers};
