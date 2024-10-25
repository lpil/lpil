use crate::ApplicationOptions;
use chrono::SubsecRound;
use std::io::Write;
use std::path::PathBuf;
use std::u64;

pub struct System {
    hostname: String,
    iteration: u64,
    system: sysinfo::System,
    discs: sysinfo::Disks,
    outfile: PathBuf,
}

impl System {
    pub fn new(options: &ApplicationOptions) -> Self {
        let hostname = sysinfo::System::host_name().expect("Unable to determine hostname");
        let mut system = sysinfo::System::new();
        let discs = sysinfo::Disks::new_with_refreshed_list();

        // A few readings are required in order to get accurate CPU data
        for _ in 0..3 {
            std::thread::sleep(sysinfo::MINIMUM_CPU_UPDATE_INTERVAL);
            system.refresh_cpu_usage();
        }

        Self {
            hostname,
            system,
            discs,
            iteration: 0,
            outfile: options.outfile.clone(),
        }
    }

    pub fn tick(&mut self) {
        println!("Sample {}", self.iteration);
        let sample = self.capture();
        self.append_sample_to_disc_log(sample);
        self.iteration += 1;
    }

    fn capture(&mut self) -> Sample {
        self.system.refresh_cpu_usage();
        self.system.refresh_memory();
        self.discs.refresh();
        let now = chrono::offset::Utc::now().trunc_subsecs(0);
        let load_avg = sysinfo::System::load_average();
        Sample::capture(now, &self.system, &load_avg, &self.discs)
    }

    fn append_sample_to_disc_log(&self, sample: Sample) {
        let mut file = std::fs::OpenOptions::new()
            .write(true)
            .append(true)
            .create(true)
            .open(&self.outfile)
            .expect("Could not open outfile for appending");
        let mut json = serde_json::to_string(&sample).expect("Could not serialize JSON");
        json.push('\n');
        file.write(json.as_str().as_bytes())
            .expect("Could not write to outfile");
    }
}

#[derive(Debug, serde::Serialize)]
#[serde(rename_all = "kebab-case")]
pub struct Sample {
    time: chrono::DateTime<chrono::Utc>,
    cpu_load_one: f64,
    cpu_load_five: f64,
    cpu_load_fifteen: f64,
    cpu_usage_global: f32,
    cpu_usage: Vec<f32>,
    memory_total: u64,
    memory_available: u64,
    memory_used: u64,
    swap_total: u64,
    swap_used: u64,
    discs: Vec<DiscSample>,
}

impl Sample {
    fn capture(
        time: chrono::DateTime<chrono::Utc>,
        system: &sysinfo::System,
        load_avg: &sysinfo::LoadAvg,
        discs: &sysinfo::Disks,
    ) -> Self {
        let cpu_usage = system
            .cpus()
            .into_iter()
            .map(|cpu| cpu.cpu_usage())
            .collect();
        Sample {
            time,
            cpu_load_one: load_avg.one,
            cpu_load_five: load_avg.five,
            cpu_load_fifteen: load_avg.fifteen,
            cpu_usage_global: system.global_cpu_usage(),
            cpu_usage,
            discs: discs.iter().map(DiscSample::capture).collect(),
            memory_total: system.total_memory(),
            memory_available: system.available_memory(),
            memory_used: system.used_memory(),
            swap_total: system.total_swap(),
            swap_used: system.used_swap(),
        }
    }
}

#[derive(Debug, serde::Serialize)]
#[serde(rename_all = "kebab-case")]
pub struct DiscSample {
    mount_point: PathBuf,
    total_space_bytes: u64,
    available_space_bytes: u64,
}

impl DiscSample {
    fn capture(disc: &sysinfo::Disk) -> Self {
        Self {
            mount_point: disc.mount_point().to_path_buf(),
            total_space_bytes: disc.total_space(),
            available_space_bytes: disc.available_space(),
        }
    }
}
