let typeDef = {|

type Query {
  episodes: [Episode!]!
  favoriteEpisode: Episode!
  numEpisodes: Int!
}

|};

let resolvers = {
  "episodes": Episode.all,
  "favoriteEpisode": Episode.favourite,
  "numEpisodes": Episode.count
};
