use crate::survey::Database;
use crate::survey::{Feedback, Survey};
use crate::web::errors::Errors;
use bytes::buf::Buf;
use warp::{body::FullBody, http::Response};

#[derive(Debug, serde::Serialize)]
pub struct IndexBody {
    pub surveys: Vec<Survey>,
}

pub fn index(db: Database) -> impl warp::Reply {
    let body = IndexBody {
        surveys: db.all_surveys(),
    };
    Response::builder()
        .status(200)
        .body(serde_json::to_string(&body).unwrap())
}

#[derive(Debug, serde::Serialize)]
pub struct FeedbackIndexBody {
    pub feedback: Vec<Feedback>,
}

pub fn feedback_index(survey_id: u32, db: Database) -> impl warp::Reply {
    match db.get_survey(survey_id) {
        None => Response::builder().status(404).body("".to_string()),

        Some(Survey { feedback, .. }) => Response::builder()
            .status(200)
            .body(serde_json::to_string(&FeedbackIndexBody { feedback }).unwrap()),
    }
}

pub fn create_feedback(survey_id: u32, json: FullBody, db: Database) -> impl warp::Reply {
    use crate::survey::InsertFeedbackError::*;

    let feedback = match serde_json::from_slice(json.bytes()) {
        Ok(feedback) => feedback,
        Err(e) => {
            let errors = Errors::single("feedback".to_string(), e.to_string());
            return Response::builder()
                .status(400)
                .body(serde_json::to_string(&errors).unwrap());
        }
    };

    match db.insert_feedback(survey_id, feedback) {
        Ok(()) => Response::builder().status(201).body("".to_string()),
        Err(NotFound) => Response::builder().status(404).body("".to_string()),
    }
}
