package main

import (
	"testing"
)

func TestAdd(t *testing.T) {
	cases := []struct {
		a        int
		b        int
		expected int
	}{
		{1, 1, 2},
		{1, 2, 3},
		{0, 0, 0},
		{-2, 1, -1},
	}

	for _, testCase := range cases {
		result := Add(testCase.a, testCase.b)
		if result != testCase.expected {
			t.Errorf(
				"Add(%d, %d) expected %d got %d",
				testCase.a,
				testCase.b,
				testCase.expected,
				result,
			)
		}
	}
}
