// use crate::survey::{Feedback, Survey};
// use juniper::EmptyMutation;

// pub fn filter() -> warp::filters::BoxedFilter<(warp::http::response::Response<Vec<u8>>,)> {
//     use warp::Filter;
//     let ctx = Ctx {};
//     let state = warp::any().map(move || ctx.clone());
//     let schema = Schema::new(Query, EmptyMutation::<Ctx>::new());
//     juniper_warp::make_graphql_filter(schema, state.boxed())
// }

// // Arbitrary context data.
// #[derive(Debug, Clone)]
// struct Ctx {}

// impl juniper::Context for Ctx {}

// struct Query;

// #[juniper::object]
// /// A request for feedback, either happy, meh, or sad.
// impl Survey {
//     fn id(&self) -> i32 {
//         self.id as i32
//     }

//     /// When the survey was created.
//     fn date(&self) -> chrono::DateTime<chrono::Utc> {
//         self.date
//     }

//     /// A concise title for the survey.
//     fn title(&self) -> &str {
//         self.title.as_str()
//     }

//     /// Extra detail on the survey.
//     fn description(&self) -> &Option<String> {
//         &self.description
//     }

//     /// A colour which may be used to accent the survey when displayed to the user.
//     fn colour(&self) -> &Option<String> {
//         &self.colour
//     }

//     /// Tags associated with the survey. These may indicate which groups the survey are of interest
//     /// to.
//     fn tags(&self) -> &Vec<String> {
//         &self.tags
//     }

//     /// Feedback submitted for the survey.
//     fn feedback(&self) -> &Vec<Feedback> {
//         &self.feedback
//     }
// }

// #[juniper::object(
//     Context = Ctx,
// )]
// impl Query {
//     fn api_version() -> &str {
//         "1.0"
//     }

//     fn all_surveys() -> Vec<Survey> {
//         crate::survey::all_surveys()
//     }
// }

// // A root schema consists of a query and a mutation.
// // Request queries can be executed against a RootNode.
// type Schema = juniper::RootNode<'static, Query, EmptyMutation<Ctx>>;
