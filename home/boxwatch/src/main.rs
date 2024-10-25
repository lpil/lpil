// TODO: alerting via pushover

use gumdrop::Options as _;
use std::{path::PathBuf, time::Duration};
mod system;

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
    #[options(help = "print help message")]
    help: bool,

    #[options(help = "Number of seconds between samples taken", required)]
    interval: u64,

    #[options(help = "The JSONC file to append sample data to", required)]
    outfile: PathBuf,
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
