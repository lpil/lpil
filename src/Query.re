let getEpisodes () => [|Episode.hope, Episode.empire, Episode.jedi|];

let getFavoriteEpisode () => Episode.empire;

let typeDef = "type Query { episodes: [Episode!]!, favoriteEpisode: Episode! }";

let resolvers = {"episodes": getEpisodes, "favoriteEpisode": getFavoriteEpisode};
