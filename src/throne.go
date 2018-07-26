package main

import (
	"encoding/binary"
	"math"
	"os"
)

const Max16BitSignedIntAsFloat = 32767.0
const SampleRate = 44100.0
const TwoPi = 3.141592 * 2.0 // One full rotation of a sine wave
const SampleStep = TwoPi / SampleRate

var frequency = 110.0
var sampleClock = 0.0

func main() {
	// need some working memory to turn our sine output into binary every
	// moment-needs to be 16 bits 2 bytes long
	buffer := make([]byte, 2)

	// by continuously printing nextsample, we produce a stream of data
	// approximating a sine wave make voodoo float number into integer
	// for makes a function loop forever
	for {
		sample := int16(nextSample())

		// write that integer into binary for aplay to use
		binary.LittleEndian.PutUint16(buffer[0:], uint16(sample))
		binary.Write(os.Stdout, binary.LittleEndian, buffer)
	}
}

// from a sample (sine output) produce another sample, a small step in rad arg
// of sine later define by sample clock.
// float64 is the memory format used to record the output of sine
func nextSample() float64 {
	sampleClock = math.Mod((sampleClock + 1.0), SampleRate)
	sample := math.Sin(sampleClock * frequency * SampleStep)
	return sample * Max16BitSignedIntAsFloat
}
