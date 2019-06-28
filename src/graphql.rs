use juniper::EmptyMutation;

pub fn filter() -> warp::filters::BoxedFilter<(warp::http::response::Response<Vec<u8>>,)> {
    use warp::Filter;
    let ctx = Ctx {};
    let state = warp::any().map(move || ctx.clone());
    let schema = Schema::new(Query, EmptyMutation::<Ctx>::new());
    juniper_warp::make_graphql_filter(schema, state.boxed())
}

// Arbitrary context data.
#[derive(Debug, Clone)]
struct Ctx {}

impl juniper::Context for Ctx {}

struct Query;

#[juniper::object(
    Context = Ctx,
)]
impl Query {
    fn api_version() -> &str {
        "1.0"
    }
}

// A root schema consists of a query and a mutation.
// Request queries can be executed against a RootNode.
type Schema = juniper::RootNode<'static, Query, EmptyMutation<Ctx>>;
