extern crate chrono;
use chrono::*;

pub fn after(date: DateTime<UTC>) -> DateTime<UTC> {
    let gigasecond = Duration::seconds(1_000_000_000);
    date + gigasecond
}
