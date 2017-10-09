/*
    Convert a Js object into a Json object.
 */
external json_of_js : Js.t {..} => Js.Json.t = "%identity";

/*
    Google Cloud Functions HTTP handler. The entrypoint for our API.

    function handler(req, res) {
      const body = req.body || {};
      const query = body.query || "";
      const variables = body.variables || {};
      const respond = result => {
        const status = result.error ? 422 : 200;
        res.status(status).json(result);
      };
      happy.runQuery(query, variables).then(respond);
    }

 */
let handler req res => {
  let body =
    Js.Dict.get (Express.Request.asJsonObject req) "body"
    |> Js.Option.andThen ((fun x => Js.Json.decodeObject x) [@bs])
    |> Js.Option.default (Js.Dict.empty ());
  let query =
    Js.Dict.get body "query"
    |> Js.Option.andThen ((fun x => Js.Json.decodeString x) [@bs])
    |> Js.Option.default "";
  query
  |> Schema.runQuery
  |> Js.Promise.then_ (
       fun result => {
         let _ = Express.Response.sendJson res (json_of_js result);
         Js.Promise.resolve ()
       }
     )
};
