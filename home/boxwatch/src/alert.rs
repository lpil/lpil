pub struct Alert {
    last_sent_at: Option<chrono::DateTime<chrono::Utc>>,
    rate_limit_window: chrono::Duration,
    pushover: PushoverClient,
}

impl Alert {
    pub fn new(rate_limit_window: chrono::Duration, pushover: &PushoverClient) -> Self {
        Self {
            last_sent_at: None,
            rate_limit_window,
            pushover: pushover.clone(),
        }
    }

    pub fn activate(&mut self, now: chrono::DateTime<chrono::Utc>, message: &str) {
        let previous = self.last_sent_at.unwrap_or(chrono::DateTime::UNIX_EPOCH);
        let rate_limit_window_end = previous + self.rate_limit_window;
        let message = message.trim();

        eprint!("ALERT: {message}\n");
        if now > rate_limit_window_end {
            self.pushover.send_notification(message);
            self.last_sent_at = Some(now);
        }
    }
}

#[derive(Clone)]
pub struct PushoverClient {
    pushover_user: String,
    pushover_token: String,
}

impl PushoverClient {
    pub fn new(pushover_user: String, pushover_token: String) -> Self {
        Self {
            pushover_user,
            pushover_token,
        }
    }

    fn send_notification(&self, message: &str) {
        let form = form_urlencoded::Serializer::new(String::new())
            .append_pair("token", &self.pushover_token)
            .append_pair("user", &self.pushover_user)
            .append_pair("message", message)
            .finish();
        let response = minreq::post("https://api.pushover.net/1/messages.json")
            .with_body(form)
            .send()
            .expect("Could not send HTTP request to Pushover");

        if response.status_code != 200 {
            panic!("Pushover status: {}", response.status_code);
        }
    }
}
