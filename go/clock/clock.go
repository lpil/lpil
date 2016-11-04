package clock

import (
	"fmt"
)

const testVersion = 4

type Clock struct {
	hours   int
	minutes int
}

func New(hour, minute int) Clock {
	h, m := trunc(hour, minute)
	return Clock{hours: h, minutes: m}
}

func (c Clock) String() string {
	return fmt.Sprintf("%02d:%02d", c.hours, c.minutes)
}

func (c Clock) Add(minutes int) Clock {
	h, m := trunc(c.hours, c.minutes+minutes)
	return Clock{hours: h, minutes: m}
}

func trunc(hours, mins int) (int, int) {
	m := mins % 60
	h := (hours + (mins / 60)) % 24
	if m < 0 {
		m += 60
		h -= 1
	}
	if h < 0 {
		h += 24
	}
	return h, m
}
