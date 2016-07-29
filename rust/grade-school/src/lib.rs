use std::collections::HashMap;

pub type Grade = u8;
pub type Student = String;
pub type Scores = HashMap<Grade, Vec<Student>>;

pub struct School {
    scores: Scores,
}

impl School {
    pub fn new() -> School {
        School { scores: HashMap::new() }
    }

    pub fn add(&mut self, grade: Grade, student: &str) {
        let students = self.scores
            .entry(grade)
            .or_insert(vec![]);
        students.push(student.to_string());
        students.sort();
    }

    pub fn grades(&self) -> Vec<Grade> {
        let mut grades = self.scores
            .keys()
            .cloned()
            .collect::<Vec<Grade>>();
        grades.sort();
        grades
    }

    pub fn grade(&self, g: Grade) -> Option<&Vec<Student>> {
        self.scores.get(&g)
    }
}
