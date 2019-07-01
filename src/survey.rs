use chrono::prelude::*;
use std::sync::{Arc, RwLock};

#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize, juniper::GraphQLObject)]
pub struct Feedback {
    pub mood: Mood,
}

#[serde(rename_all = "lowercase")]
#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize, juniper::GraphQLEnum)]
pub enum Mood {
    Happy,
    Meh,
    Sad,
}

#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize)]
pub struct Survey {
    pub id: u32,
    pub date: chrono::DateTime<chrono::Utc>,
    pub title: String,
    pub description: Option<String>,
    pub colour: Option<String>,
    pub tags: Vec<String>,
    #[serde(skip_serializing)]
    #[serde(default = "Vec::new")]
    pub feedback: Vec<Feedback>,
}

// Data required to insert a new Survey
#[derive(Debug, Clone, PartialEq, serde::Deserialize)]
pub struct InsertSurvey {
    pub title: String,
    pub description: Option<String>,
    pub colour: Option<String>,
    pub tags: Vec<String>,
}

pub enum InsertFeedbackError {
    NotFound,
}

// So we don't have to tackle a database just yet, we'll just use
// a simple in-memory DB, a vector synchronized by a mutex.
#[derive(Debug, Clone)]
pub struct Database {
    arc: Arc<RwLock<Vec<Survey>>>,
}

impl Database {
    pub fn new() -> Self {
        Database {
            arc: Arc::new(RwLock::new(vec![])),
        }
    }

    pub fn seed(&self) {
        let mut lock = self.arc.write().unwrap();
        *lock = vec![
            Survey {
                title: "The Iron Throne".to_string(),
                description: Some("How do you feel about the final GoT episode?".to_string()),
                tags: vec!["Game of Thrones".to_string(), "Finale".to_string()],
                colour: Some("#ffccbb".to_string()),
                id: 1,
                date: DateTime::from_utc(NaiveDate::from_ymd(2016, 7, 8).and_hms(0, 0, 0), Utc),
                feedback: vec![
                    Feedback { mood: Mood::Happy },
                    Feedback { mood: Mood::Happy },
                    Feedback { mood: Mood::Sad },
                    Feedback { mood: Mood::Sad },
                    Feedback { mood: Mood::Meh },
                    Feedback { mood: Mood::Happy },
                ],
            },
            Survey {
                title: "Did you see that ludicrous display last night?".to_string(),
                description: Some(
                    "What was Wenger thinking sending Walcott on that early?".to_string(),
                ),
                tags: vec![
                    "football".to_string(),
                    "sport".to_string(),
                    "totally-normal".to_string(),
                ],
                colour: Some("#99ccee".to_string()),
                id: 3,
                date: DateTime::from_utc(NaiveDate::from_ymd(2016, 7, 8).and_hms(0, 0, 0), Utc),
                feedback: vec![
                    Feedback { mood: Mood::Meh },
                    Feedback { mood: Mood::Happy },
                    Feedback { mood: Mood::Sad },
                    Feedback { mood: Mood::Happy },
                    Feedback { mood: Mood::Happy },
                    Feedback { mood: Mood::Sad },
                    Feedback { mood: Mood::Happy },
                    Feedback { mood: Mood::Sad },
                ],
            },
            Survey {
                title: "Rate my cat".to_string(),
                description: Some("Their name is fluffy".to_string()),
                tags: vec!["cat".to_string(), "cute".to_string()],
                colour: Some("#99ccee".to_string()),
                id: 4,
                date: DateTime::from_utc(NaiveDate::from_ymd(2016, 7, 8).and_hms(0, 0, 0), Utc),
                feedback: vec![
                    Feedback { mood: Mood::Happy },
                    Feedback { mood: Mood::Happy },
                    Feedback { mood: Mood::Happy },
                ],
            },
            Survey {
                title: "Election results".to_string(),
                description: None,
                tags: vec!["politics".to_string()],
                colour: None,
                id: 5,
                date: DateTime::from_utc(NaiveDate::from_ymd(2016, 7, 8).and_hms(0, 0, 0), Utc),
                feedback: vec![
                    Feedback { mood: Mood::Meh },
                    Feedback { mood: Mood::Sad },
                    Feedback { mood: Mood::Happy },
                    Feedback { mood: Mood::Sad },
                    Feedback { mood: Mood::Happy },
                    Feedback { mood: Mood::Happy },
                ],
            },
            Survey {
                title: "Conference call audio".to_string(),
                description: Some(
                    "Was the audio quality good for the last NDAP remote meeting?".to_string(),
                ),
                tags: vec!["ndap".to_string(), "av".to_string(), "meeting".to_string()],
                colour: None,
                id: 6,
                date: DateTime::from_utc(NaiveDate::from_ymd(2016, 7, 8).and_hms(0, 0, 0), Utc),
                feedback: vec![
                    Feedback { mood: Mood::Meh },
                    Feedback { mood: Mood::Meh },
                    Feedback { mood: Mood::Happy },
                ],
            },
        ];
    }

    pub fn get_survey(&self, id: u32) -> Option<Survey> {
        self.arc
            .read()
            .unwrap()
            .iter()
            .find(|s| s.id == id)
            .map(|s| (*s).clone())
    }

    pub fn all_surveys(&self) -> Vec<Survey> {
        (*self.arc.read().unwrap()).clone()
    }

    pub fn insert_feedback(&self, id: u32, feedback: Feedback) -> Result<(), InsertFeedbackError> {
        match self.arc.write().unwrap().iter_mut().find(|s| s.id == id) {
            Some(survey) => Ok(survey.feedback.push(feedback)),
            None => Err(InsertFeedbackError::NotFound),
        }
    }

    pub fn insert_survey(&self, survey: InsertSurvey) -> Survey {
        let mut lock = self.arc.write().unwrap();
        let survey = Survey {
            title: survey.title,
            description: survey.description,
            colour: survey.colour,
            tags: survey.tags,
            date: chrono::Utc::now(),
            feedback: vec![],
            id: (lock.len() + 1) as u32,
        };
        lock.push(survey.clone());
        survey
    }
}
