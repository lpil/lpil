package main

import (
	"bytes"
	"encoding/binary"
	"fmt"
	"math"

	"github.com/yobert/alsa"
)

var sampleRate int
var frequency = 440.
var phase = 0.

const twoPi = 3.141592 * 2. // One full rotation of a sine wave

func main() {
	cards, err := alsa.OpenCards()
	if err != nil {
		fmt.Println(err)
		return
	}
	defer alsa.CloseCards(cards)

	for _, card := range cards {
		fmt.Printf("Attempting to play via card %s\n", card)

		if err := playCard(card); err != nil {
			fmt.Printf("Unable to play: %v\n", err)
		}
	}
}

func playCard(card *alsa.Card) error {
	devices, err := card.Devices()
	if err != nil {
		return err
	}
	for _, device := range devices {
		if device.Type != alsa.PCM || !device.Play {
			continue
		}

		if err := playDevice(device); err != nil {
			fmt.Printf("error when beeping device: %v\n", err)
		}
	}
	return nil
}

func playDevice(device *alsa.Device) error {
	var err error

	if err = device.Open(); err != nil {
		return err
	}
	defer device.Close() // Cleanup device when done

	channels, err := device.NegotiateChannels(1, 2)
	if err != nil {
		return err
	}

	sampleRate, err = device.NegotiateRate(44100, 48000)
	fmt.Printf("%d\n", sampleRate)
	if err != nil {
		return err
	}

	format, err := device.NegotiateFormat(alsa.S16_LE, alsa.S32_LE)
	fmt.Printf("%d\n", format)
	if err != nil {
		return err
	}

	// We adjust the buffer so it's of minimal size (period * 2) since it appear ALSA won't
	// start playback until the buffer has been filled to a certain degree and the automatic
	// buffer size can be quite large.
	// Some devices only accept even periods while others want powers of 2.
	wantPeriodSize := 2048 // 46ms @ 44100Hz

	periodSize, err := device.NegotiatePeriodSize(wantPeriodSize)
	if err != nil {
		return err
	}

	bufferSize, err := device.NegotiateBufferSize(wantPeriodSize * 2)
	if err != nil {
		return err
	}

	if err = device.Prepare(); err != nil {
		return err
	}

	fmt.Printf("Negotiated parameters: %d channels, %d hz, %v, %d period size, %d buffer size\n",
		channels, sampleRate, format, periodSize, bufferSize)

	for {
		var buf bytes.Buffer

		for i := 0; i < periodSize; i++ {
			sample := nextSample() * 0.1

			switch format {
			case alsa.S16_LE:
				scaledSample := int16(sample * math.MaxInt16)

				for c := 0; c < channels; c++ {
					binary.Write(&buf, binary.LittleEndian, scaledSample)
				}

			case alsa.S32_LE:
				scaledSample := int32(sample * math.MaxInt32)

				for c := 0; c < channels; c++ {
					binary.Write(&buf, binary.LittleEndian, scaledSample)
				}

			default:
				return fmt.Errorf("Unhandled sample format: %v", format)
			}

		}

		if err := device.Write(buf.Bytes(), periodSize); err != nil {
			return err
		}
	}

	return nil
}

// from a sample (sine output) produce another sample, a small step in rad arg
// of sine later define by sample clock.
// float64 is the memory format used to record the output of sine
func nextSample() float64 {
	sample := math.Sin(phase * twoPi)
	phase = math.Mod(phase+frequency/float64(sampleRate), 1.)
	return sample
}
