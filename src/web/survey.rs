use crate::survey::{Feedback, Survey};
use warp::http::Response;

#[derive(Debug, serde::Serialize)]
pub struct IndexBody {
    pub surveys: Vec<Survey>,
}

pub fn index() -> impl warp::Reply {
    let body = IndexBody {
        surveys: crate::survey::all_surveys(),
    };
    Response::builder()
        .status(200)
        .body(serde_json::to_string(&body).unwrap())
}

#[derive(Debug, serde::Serialize)]
pub struct FeedbackIndexBody {
    pub feedback: Vec<Feedback>,
}

pub fn feedback_index(survey_id: u32) -> impl warp::Reply {
    match crate::survey::get_survey(survey_id) {
        None => Response::builder().status(404).body("".to_string()),

        Some(Survey { feedback, .. }) => Response::builder()
            .status(200)
            .body(serde_json::to_string(&FeedbackIndexBody { feedback }).unwrap()),
    }
}
