use juniper::{EmptyMutation, FieldResult};

pub fn filter() -> warp::filters::BoxedFilter<(warp::http::response::Response<Vec<u8>>,)> {
    use warp::Filter;
    let state = warp::any().map(move || Ctx(Episode::NewHope));
    let schema = Schema::new(Query, EmptyMutation::<Ctx>::new());
    juniper_warp::make_graphql_filter(schema, state.boxed())
}

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
