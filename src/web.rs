mod health;
mod survey;

use warp::http::Response;
use warp::Filter;

pub fn routes() -> impl warp::Filter<Extract = (impl warp::Reply), Error = warp::Rejection> + Clone
{
    use warp::path::*;
    use warp::{any, get2 as get, post2 as post};

    // GET /-/ready
    let ready_check = get()
        .and(path("-"))
        .and(path("ready"))
        .and(end())
        .map(crate::web::health::readiness);

    // GET /-/health
    let health_check = get()
        .and(path("-"))
        .and(path("health"))
        .and(end())
        .map(crate::web::health::health);

    // GET /
    let graphiql_ui = get()
        .and(end())
        .and(juniper_warp::graphiql_filter("/graphql"));

    // POST /graphql
    let graphql_endpoint = post()
        .and(path("graphql"))
        .and(end())
        .and(crate::graphql::filter());

    // GET /surveys
    let surveys_index = get()
        .and(path("surveys"))
        .and(end())
        .map(crate::web::survey::index);

    // GET /surveys/:id/feedback
    let show_survey_feedback = get()
        .and(path("surveys"))
        .and(param())
        .and(path("feedback"))
        .and(end())
        .map(crate::web::survey::feedback_index);

    let not_found = any().map(|| Response::builder().status(404).body("Not found"));

    graphiql_ui
        .or(health_check)
        .or(ready_check)
        .or(surveys_index)
        .or(show_survey_feedback)
        .or(graphql_endpoint)
        .or(not_found)
}

#[test]
fn graphiql_test() {
    let res = warp::test::request()
        .path("/")
        .method("GET")
        .reply(&routes());
    assert_eq!(200, res.status());
}

#[test]
fn not_found_test() {
    let res = warp::test::request()
        .path("/whatever")
        .method("GET")
        .reply(&routes());
    assert_eq!(404, res.status());
}

#[test]
fn ready_test() {
    let res = warp::test::request()
        .path("/-/ready")
        .method("GET")
        .reply(&routes());
    assert_eq!(200, res.status());
}

#[test]
fn health_test() {
    let res = warp::test::request()
        .path("/-/health")
        .method("GET")
        .reply(&routes());
    assert_eq!(200, res.status());
}

#[test]
fn surveys_index_test() {
    crate::survey::dangerously_dump_and_seed_database();
    let res = warp::test::request()
        .path("/surveys")
        .method("GET")
        .reply(&routes());
    assert_eq!(200, res.status());
    assert_eq!(
        serde_json::json!({
            "surveys": [
                {
                    "id": 1,
                    "date": "2016-07-08T00:00:00Z",
                    "title": "The Iron Throne",
                    "description": "How do you feel about the final GoT episode?",
                    "colour": "#ffccbb",
                    "tags": [
                        "Game of Thrones",
                        "Finale"
                    ]
                },
                {
                    "id": 3,
                    "date": "2016-07-08T00:00:00Z",
                    "title": "Did you see that ludicrous display last night?",
                    "description": "What was Wenger thinking sending Walcott on that early?",
                    "colour": "#99ccee",
                    "tags": [
                        "football",
                        "sport",
                        "totally-normal"
                    ]
                },
                {
                    "id": 4,
                    "date": "2016-07-08T00:00:00Z",
                    "title": "Rate my cat",
                    "description": "Their name is fluffy",
                    "colour": "#99ccee",
                    "tags": [
                        "cat",
                        "cute"
                    ]
                },
                {
                    "id": 5,
                    "date": "2016-07-08T00:00:00Z",
                    "title": "Election results",
                    "description": null,
                    "colour": null,
                    "tags": [
                        "politics"
                    ]
                },
                {
                    "id": 6,
                    "date": "2016-07-08T00:00:00Z",
                    "title": "Conference call audio",
                    "description": "Was the audio quality good for the last NDAP remote meeting?",
                    "colour": null,
                    "tags": [
                        "ndap",
                        "av",
                        "meeting"
                    ]
                }
            ]
        }),
        serde_json::from_str::<serde_json::Value>(&String::from_utf8_lossy(res.body())).unwrap()
    );
}

#[test]
fn surveys_feedback_index_ok_test() {
    crate::survey::dangerously_dump_and_seed_database();
    let res = warp::test::request()
        .path("/surveys/1/feedback")
        .method("GET")
        .reply(&routes());
    assert_eq!(200, res.status());
    assert_eq!(
        serde_json::json!({
            "feedback": [
                {"mood": "happy"},
                {"mood": "happy"},
                {"mood": "sad"},
                {"mood": "sad"},
                {"mood": "meh"},
                {"mood": "happy"},
            ]
        }),
        serde_json::from_str::<serde_json::Value>(&String::from_utf8_lossy(res.body())).unwrap()
    );
}

#[test]
fn surveys_feedback_index_not_found_test() {
    crate::survey::dangerously_dump_and_seed_database();
    let res = warp::test::request()
        .path("/surveys/0/feedback")
        .method("GET")
        .reply(&routes());
    assert_eq!(404, res.status());
    assert_eq!("", String::from_utf8_lossy(res.body()));
}
