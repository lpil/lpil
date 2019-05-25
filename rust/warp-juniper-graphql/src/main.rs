use warp::http::Response;
use warp::{http::StatusCode, Filter};

fn main() {
    // Show info level logs by default
    if std::env::var_os("RUST_LOG").is_none() {
        std::env::set_var("RUST_LOG", "berry=info");
    }
    pretty_env_logger::init();

    let port = std::env::var("RUST_LOG")
        .ok()
        .and_then(|s| s.parse().ok())
        .unwrap_or(3000);

    let stack = routes().with(warp::log("berry"));

    log::info!("Starting on 0.0.0.0:{}", port);
    warp::serve(stack).run(([0, 0, 0, 0], port));
}

fn routes() -> impl warp::Filter<Extract = (impl warp::Reply), Error = warp::Rejection> + Clone {
    use warp::path::*;
    use warp::{any, get2 as get};

    // GET /-/ready
    let ready_check = get()
        .and(path("-"))
        .and(path("ready"))
        .and(end())
        .map(ready_check);

    // GET /-/health
    let health_check = get()
        .and(path("-"))
        .and(path("health"))
        .and(end())
        .map(health_check);

    // GET /graphiql/...
    let graphiql_ui = get()
        .and(end())
        .and(juniper_warp::graphiql_filter("/graphql"));

    // * /graphql
    let state = warp::any().map(move || Ctx(Episode::NewHope));
    let schema = Schema::new(Query, EmptyMutation::<Ctx>::new());
    let graphql_endpoint =
        path("graphql").and(juniper_warp::make_graphql_filter(schema, state.boxed()));

    let not_found = any().map(not_found);

    health_check
        .or(ready_check)
        .or(graphiql_ui)
        .or(graphql_endpoint)
        .or(not_found)
}

fn not_found() -> impl warp::Reply {
    Response::builder().status(404).body("Not found")
}

fn ready_check() -> impl warp::Reply {
    StatusCode::OK
}

fn health_check() -> impl warp::Reply {
    StatusCode::OK
}

// copypasta

use juniper::{EmptyMutation, FieldResult};

#[derive(juniper::GraphQLEnum, Debug, Clone, Copy)]
enum Episode {
    NewHope,
    Empire,
    Jedi,
}

// Arbitrary context data.
#[derive(Debug, Clone)]
struct Ctx(Episode);

impl juniper::Context for Ctx {}

struct Query;

#[juniper::object(
    Context = Ctx,
)]
impl Query {
    fn api_version() -> &str {
        "1.0"
    }

    fn favorite_episode(context: &Ctx) -> FieldResult<Episode> {
        Ok(context.0)
    }

    fn all_episodes() -> Vec<Episode> {
        vec![Episode::NewHope, Episode::Empire, Episode::Jedi]
    }
}

// A root schema consists of a query and a mutation.
// Request queries can be executed against a RootNode.
type Schema = juniper::RootNode<'static, Query, EmptyMutation<Ctx>>;
