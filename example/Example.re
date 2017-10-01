type episode = {
  name: string,
  number: int
};

type context_data = {current_user: user};

/*
   Schema/Nodes definition.

   These functions build up a graph data structure that describes the schema.
   We can later traverse this graph in order to build the schema.

 */
let episode_type =
  GraphQl.(
    type_ "Episode"
    |> field "name" String (fun e c => e.name)
    |> field "number" String (fun e c => e.number)
  );

let schema =
  GraphQl.(
    schema
    |> field "numEpisodes" Int Episode.count
    |> field "favoriteEpisode" episode_type Episode.favourite
    |> field "episodes" (List episode_type) Episode.all
  );

/*
   Query execution.

   Schema is transformed into the `executable_schema` once as it's potentially
   an expensive operation.

 */
let executable_schema = GraphQl.make_executable_schema schema;

let run_query current_user query => GraphQl.run_query {current_user: user} query;
