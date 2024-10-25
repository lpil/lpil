use crate::{
    alert::{Alert, PushoverClient},
    system::Sample,
};

const RATE_LIMIT_WINDOW: chrono::Duration = chrono::Duration::minutes(30);

pub struct CheckMemoryUsage {
    alert: Alert,
    which: WhichMemory,
}

impl CheckMemoryUsage {
    pub fn new(pushover: &PushoverClient, which: WhichMemory) -> Self {
        let alert = Alert::new(RATE_LIMIT_WINDOW, pushover);
        Self { alert, which }
    }

    pub fn check(&mut self, sample: &Sample) {
        let (total, used, which) = match self.which {
            WhichMemory::Ram => (sample.memory_total, sample.memory_used, "RAM"),
            WhichMemory::Swap => (sample.swap_total, sample.swap_used, "Swap"),
        };
        let percent = ((used as f64 / total as f64) * 100.0) as u8;

        if percent >= 90 {
            let total = total as f64 / 1024. / 1024. / 1024.;
            let used = used as f64 / 1024. / 1024. / 1024.;
            let message = format!("{which} usage {percent}% ({used:.2} / {total:.2} GB)");
            self.alert.activate(sample.time, &message);
        }
    }
}

pub enum WhichMemory {
    Ram,
    Swap,
}
