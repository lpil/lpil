use std::f64;

pub const SAMPLE_RATE: f64 = 44100.0;
pub const TWO_PI: f64 = f64::consts::PI * 2.0;

type Sample = f64;

#[derive(Debug)]
pub struct Sine {
    frequency: u64,
    offset: f64,
}

impl Sine {
    pub fn get_sample(&self) -> Sample {
        (self.offset * TWO_PI).sin()
    }

    pub fn step(&mut self) {
        self.offset += self.frequency as f64 / SAMPLE_RATE;
    }
}

/// Create a new Sine wave oscillator
///
pub fn new(frequency: u64) -> Sine {
    Sine {
        frequency: frequency,
        offset: 0.0,
    }
}


#[cfg(test)]
mod tests {
    use std::f64;
    use super::*;

    fn nearly_eq(num: f64, target: f64) -> bool {
        (num - target).abs() <= (f64::EPSILON * 2.0)
    }

    #[test]
    fn test_new() {
        let freq = SAMPLE_RATE as u64 / 4;
        let mut sine = new(freq);
        assert_eq!(sine.frequency, freq);
        assert!(nearly_eq(sine.get_sample(), 0.0));
        sine.step();
        assert!(nearly_eq(sine.get_sample(), 1.0));
        sine.step();
        assert!(nearly_eq(sine.get_sample(), 0.0));
        sine.step();
        assert!(nearly_eq(sine.get_sample(), -1.0));
        sine.step();
        assert!(nearly_eq(sine.get_sample(), 0.0));
        sine.step();
        assert!(nearly_eq(sine.get_sample(), 1.0));
        sine.step();
        assert!(nearly_eq(sine.get_sample(), 0.0));
        sine.step();
        assert!(nearly_eq(sine.get_sample(), -1.0));
    }
}
