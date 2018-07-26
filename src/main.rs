//! Outputs a 16 bit little endian 44100 sine wave over stdout.
//!
//! Pipe into `aplay -f cd` on Linux to run.
//!
extern crate byteorder;

use byteorder::{LittleEndian, WriteBytesExt};
use std::io;

const MAX_16BIT_SIGNED_INT_AS_FLOAT: f32 = 32767.0;
const SAMPLE_RATE: f32 = 44100.0;
const TWO_PI: f32 = 3.141592 * 2.0;

fn main() {
    let mut sample_clock = 0f32;

    let mut next_sample = || {
        sample_clock = (sample_clock + 1.0) % SAMPLE_RATE;
        let sample = (sample_clock * 110.0 * TWO_PI / SAMPLE_RATE).sin();
        let scaled_sample = sample * MAX_16BIT_SIGNED_INT_AS_FLOAT;
        scaled_sample
    };

    loop {
        let sample = next_sample() as i16;
        io::stdout().write_i16::<LittleEndian>(sample).unwrap();
    }
}
