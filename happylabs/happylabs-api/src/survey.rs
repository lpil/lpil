use chrono::prelude::*;
use std::sync::{Arc, RwLock};

#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize, juniper::GraphQLObject)]
pub struct Feedback {
    pub mood: Mood,
    pub inserted_at: chrono::DateTime<chrono::Utc>,
}

#[derive(Debug, Clone, PartialEq, serde::Deserialize, juniper::GraphQLObject)]
pub struct InsertFeedback {
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
                title: "Reflection".to_string(),
                description: Some("Reflection next steps".to_string()),
                tags: vec!["ndap".to_string(), "meeting".to_string()],
                colour: Some("#ffccbb".to_string()),
                id: 1,
                date: DateTime::from_utc(NaiveDate::from_ymd(2016, 7, 8).and_hms(0, 0, 0), Utc),
                feedback: vec![
                    Feedback {
                        inserted_at: DateTime::from_utc(
                            NaiveDate::from_ymd(2019, 6, 29).and_hms(12, 10, 0),
                            Utc,
                        ),
                        mood: Mood::Happy,
                    },
                    Feedback {
                        inserted_at: DateTime::from_utc(
                            NaiveDate::from_ymd(2019, 6, 29).and_hms(12, 12, 0),
                            Utc,
                        ),
                        mood: Mood::Happy,
                    },
                    Feedback {
                        inserted_at: DateTime::from_utc(
                            NaiveDate::from_ymd(2019, 6, 29).and_hms(12, 15, 0),
                            Utc,
                        ),
                        mood: Mood::Sad,
                    },
                    Feedback {
                        inserted_at: DateTime::from_utc(
                            NaiveDate::from_ymd(2019, 6, 29).and_hms(12, 19, 0),
                            Utc,
                        ),
                        mood: Mood::Happy,
                    },
                    Feedback {
                        inserted_at: DateTime::from_utc(
                            NaiveDate::from_ymd(2019, 6, 29).and_hms(12, 24, 0),
                            Utc,
                        ),
                        mood: Mood::Meh,
                    },
                    Feedback {
                        inserted_at: DateTime::from_utc(
                            NaiveDate::from_ymd(2019, 6, 29).and_hms(12, 30, 0),
                            Utc,
                        ),
                        mood: Mood::Happy,
                    },
                ],
            },
            Survey {
                title: "JIRA Workflow".to_string(),
                description: Some("Are we happy with the new JIRA workflow".to_string()),
                tags: vec![
                    "process".to_string(),
                    "ndap".to_string(),
                    "jira".to_string(),
                ],
                colour: Some("#99ccee".to_string()),
                id: 3,
                date: DateTime::from_utc(NaiveDate::from_ymd(2019, 6, 28).and_hms(0, 0, 0), Utc),
                feedback: vec![
                    Feedback {
                        inserted_at: DateTime::from_utc(
                            NaiveDate::from_ymd(2019, 6, 29).and_hms(11, 10, 0),
                            Utc,
                        ),
                        mood: Mood::Meh,
                    },
                    Feedback {
                        inserted_at: DateTime::from_utc(
                            NaiveDate::from_ymd(2019, 6, 29).and_hms(11, 50, 0),
                            Utc,
                        ),
                        mood: Mood::Happy,
                    },
                    Feedback {
                        inserted_at: DateTime::from_utc(
                            NaiveDate::from_ymd(2019, 6, 29).and_hms(12, 01, 0),
                            Utc,
                        ),
                        mood: Mood::Meh,
                    },
                    Feedback {
                        inserted_at: DateTime::from_utc(
                            NaiveDate::from_ymd(2019, 6, 29).and_hms(12, 10, 0),
                            Utc,
                        ),
                        mood: Mood::Happy,
                    },
                    Feedback {
                        inserted_at: DateTime::from_utc(
                            NaiveDate::from_ymd(2019, 6, 29).and_hms(12, 11, 0),
                            Utc,
                        ),
                        mood: Mood::Happy,
                    },
                    Feedback {
                        inserted_at: DateTime::from_utc(
                            NaiveDate::from_ymd(2019, 6, 29).and_hms(12, 11, 10),
                            Utc,
                        ),
                        mood: Mood::Meh,
                    },
                    Feedback {
                        inserted_at: DateTime::from_utc(
                            NaiveDate::from_ymd(2019, 6, 30).and_hms(8, 10, 0),
                            Utc,
                        ),
                        mood: Mood::Happy,
                    },
                    Feedback {
                        inserted_at: DateTime::from_utc(
                            NaiveDate::from_ymd(2019, 6, 30).and_hms(8, 11, 0),
                            Utc,
                        ),
                        mood: Mood::Happy,
                    },
                    Feedback {
                        inserted_at: DateTime::from_utc(
                            NaiveDate::from_ymd(2019, 6, 30).and_hms(8, 33, 0),
                            Utc,
                        ),
                        mood: Mood::Sad,
                    },
                ],
            },
            Survey {
                title: "Announcements".to_string(),
                description: Some("Announcements".to_string()),
                tags: vec!["ndap".to_string()],
                colour: Some("#99ccee".to_string()),
                id: 4,
                date: DateTime::from_utc(NaiveDate::from_ymd(2016, 7, 8).and_hms(0, 0, 0), Utc),
                feedback: vec![
                    Feedback {
                        inserted_at: DateTime::from_utc(
                            NaiveDate::from_ymd(2019, 6, 29).and_hms(12, 10, 0),
                            Utc,
                        ),
                        mood: Mood::Happy,
                    },
                    Feedback {
                        inserted_at: DateTime::from_utc(
                            NaiveDate::from_ymd(2019, 6, 29).and_hms(12, 10, 0),
                            Utc,
                        ),
                        mood: Mood::Happy,
                    },
                    Feedback {
                        inserted_at: DateTime::from_utc(
                            NaiveDate::from_ymd(2019, 6, 29).and_hms(12, 10, 0),
                            Utc,
                        ),
                        mood: Mood::Happy,
                    },
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
                    Feedback {
                        inserted_at: DateTime::from_utc(
                            NaiveDate::from_ymd(2019, 5, 1).and_hms(12, 10, 0),
                            Utc,
                        ),
                        mood: Mood::Meh,
                    },
                    Feedback {
                        inserted_at: DateTime::from_utc(
                            NaiveDate::from_ymd(2019, 5, 2).and_hms(12, 10, 0),
                            Utc,
                        ),
                        mood: Mood::Meh,
                    },
                    Feedback {
                        inserted_at: DateTime::from_utc(
                            NaiveDate::from_ymd(2019, 5, 5).and_hms(12, 10, 0),
                            Utc,
                        ),
                        mood: Mood::Meh,
                    },
                    Feedback {
                        inserted_at: DateTime::from_utc(
                            NaiveDate::from_ymd(2019, 5, 7).and_hms(12, 10, 0),
                            Utc,
                        ),
                        mood: Mood::Sad,
                    },
                    Feedback {
                        inserted_at: DateTime::from_utc(
                            NaiveDate::from_ymd(2019, 5, 8).and_hms(12, 10, 0),
                            Utc,
                        ),
                        mood: Mood::Sad,
                    },
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
                    Feedback {
                        inserted_at: DateTime::from_utc(
                            NaiveDate::from_ymd(2019, 6, 29).and_hms(12, 10, 0),
                            Utc,
                        ),
                        mood: Mood::Meh,
                    },
                    Feedback {
                        inserted_at: DateTime::from_utc(
                            NaiveDate::from_ymd(2019, 6, 29).and_hms(12, 10, 0),
                            Utc,
                        ),
                        mood: Mood::Meh,
                    },
                    Feedback {
                        inserted_at: DateTime::from_utc(
                            NaiveDate::from_ymd(2019, 6, 29).and_hms(12, 10, 0),
                            Utc,
                        ),
                        mood: Mood::Happy,
                    },
                ],
            },
            Survey {
                title: "Rate my cat".to_string(),
                description: Some("Their name is fluffy".to_string()),
                tags: vec!["cute".to_string(), "cat".to_string()],
                colour: None,
                id: 7,
                date: DateTime::from_utc(NaiveDate::from_ymd(2016, 7, 8).and_hms(0, 0, 0), Utc),
                feedback: vec![],
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

    pub fn insert_feedback(
        &self,
        id: u32,
        feedback: InsertFeedback,
    ) -> Result<(), InsertFeedbackError> {
        let feedback = Feedback {
            mood: feedback.mood,
            inserted_at: chrono::Utc::now(),
        };
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
