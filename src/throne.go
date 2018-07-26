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

const Channels = 2
const BytesInSample = 2

var frequency = 110.0
var phase = 0.0

func main() {
	numSamples := int(SampleRate / frequency)
	numBytesInBuffer := numSamples * Channels * BytesInSample
	buffer := make([]byte, numBytesInBuffer)

	i := 0
	for i < numBytesInBuffer {
		sample := int16(nextSample())

		// Write left channel sample
		binary.LittleEndian.PutUint16(buffer[i:], uint16(sample))
		// Write right channel sample
		binary.LittleEndian.PutUint16(buffer[i+BytesInSample:], uint16(sample))

		i = i + 4
	}

	for {
		binary.Write(os.Stdout, binary.LittleEndian, buffer)
	}
}

// from a sample (sine output) produce another sample, a small step in rad arg
// of sine later define by sample clock.
// float64 is the memory format used to record the output of sine
func nextSample() float64 {
	sample := math.Sin(phase * TwoPi)
	// phase = math.Mod((phase + frequency/SampleRate), TwoPi)
	phase = phase + frequency/SampleRate
	return sample * Max16BitSignedIntAsFloat
}
