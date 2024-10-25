// TODO: check high CPU load
// TODO: check >90% disc usage

mod alert;
mod check_memory_usage;
mod check_swap_usage;
mod system;

use gumdrop::Options as _;
use std::{path::PathBuf, time::Duration};

fn main() {
    let options = ApplicationOptions::load();

    let mut system = system::System::new(&options);
    loop {
        system.tick();
        std::thread::sleep(Duration::from_secs(options.interval));
    }
}

#[derive(Debug, gumdrop::Options)]
struct ApplicationOptions {
    #[options(help = "Print help message")]
    help: bool,

    #[options(help = "Number of seconds between samples taken", required)]
    interval: u64,

    #[options(help = "The JSONC file to append sample data to", required)]
    outfile: PathBuf,

    #[options(help = "The Pushover user to send alerts to", required)]
    pushover_user: String,

    #[options(help = "The Pushover token to send alerts with", required)]
    pushover_token: String,
}

impl ApplicationOptions {
    fn load() -> Self {
        let args: Vec<_> = std::env::args().skip(1).collect();
        match ApplicationOptions::parse_args_default(&args) {
            Ok(opts) => opts,
            Err(error) => {
                eprintln!("{}\n\n{}", error, ApplicationOptions::usage());
                std::process::exit(2);
            }
        }
    }
}
