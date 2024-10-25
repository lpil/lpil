use crate::alert::PushoverClient;
use crate::check_memory_usage::{CheckMemoryUsage, WhichMemory};
use crate::ApplicationOptions;
use chrono::SubsecRound;
use std::io::Write;
use std::path::PathBuf;
use std::u64;

pub struct System {
    iteration: u64,
    system: sysinfo::System,
    discs: sysinfo::Disks,
    outfile: PathBuf,
    check_memory: CheckMemoryUsage,
    check_swap: CheckMemoryUsage,
}

impl System {
    pub fn new(options: &ApplicationOptions) -> Self {
        let pushover = PushoverClient::new(
            options.pushover_user.clone(),
            options.pushover_token.clone(),
        );
        let check_memory = CheckMemoryUsage::new(&pushover, WhichMemory::Ram);
        let check_swap = CheckMemoryUsage::new(&pushover, WhichMemory::Swap);

        let mut system = sysinfo::System::new();
        let discs = sysinfo::Disks::new_with_refreshed_list();

        // A few readings are required in order to get accurate CPU data
        for _ in 0..3 {
            std::thread::sleep(sysinfo::MINIMUM_CPU_UPDATE_INTERVAL);
            system.refresh_cpu_usage();
        }

        Self {
            system,
            discs,
            iteration: 0,
            outfile: options.outfile.clone(),
            check_swap,
            check_memory,
        }
    }

    pub fn tick(&mut self) {
        println!("Sample {}", self.iteration);
        let sample = self.capture();
        self.append_sample_to_disc_log(&sample);
        self.check_memory.check(&sample);
        self.check_swap.check(&sample);
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

    fn append_sample_to_disc_log(&self, sample: &Sample) {
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
    pub time: chrono::DateTime<chrono::Utc>,
    pub cpu_load_one: f64,
    pub cpu_load_five: f64,
    pub cpu_load_fifteen: f64,
    pub cpu_usage_global: f32,
    pub cpu_usage: Vec<f32>,
    pub memory_total: u64,
    pub memory_available: u64,
    pub memory_used: u64,
    pub swap_total: u64,
    pub swap_used: u64,
    pub discs: Vec<DiscSample>,
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
    pub mount_point: PathBuf,
    pub total_space_bytes: u64,
    pub available_space_bytes: u64,
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
