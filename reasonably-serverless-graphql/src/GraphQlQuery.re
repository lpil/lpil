let typeDef = {|

type Query {
  episodes: [Episode!]!
  favoriteEpisode: Episode!
  numEpisodes: Int!
  echo(phrase: String!): String!
}

|};

type echo_args = Js.t {. phrase : string};

let resolvers = {
  "episodes": Episode.all,
  "favoriteEpisode": Episode.favourite,
  "numEpisodes": Episode.count,
  "echo": fun (_root: unit) (args: echo_args) => args##phrase
};
