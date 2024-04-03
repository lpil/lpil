package check

import (
	"testing"
)

func TestCheckResultIsOk(t *testing.T) {
	tests := []struct {
		name     string
		given    CheckResult
		expected bool
	}{
		{"pass", CheckPass{}, true},
		{"fail", CheckFail{}, false},
		{"error", CheckError{}, false},
	}
	for _, tt := range tests {
		tt := tt
		t.Run(tt.name, func(t *testing.T) {
			actual := tt.given.IsOk()
			if actual != tt.expected {
				t.Errorf("\nGiven\n%#v\n\nExpected\n%#v\n\nGot\n%#v", tt.given, tt.expected, actual)
			}
		})
	}
}

func TestCheckResultString(t *testing.T) {
	tests := []struct {
		name     string
		given    CheckResult
		expected string
	}{
		{"pass", CheckPass{}, "pass"},
		{"fail", CheckFail{}, "fail"},
		{"error", CheckError{}, "error"},
	}
	for _, tt := range tests {
		tt := tt
		t.Run(tt.name, func(t *testing.T) {
			actual := tt.given.String()
			if actual != tt.expected {
				t.Errorf("\nGiven\n%#v\n\nExpected\n%#v\n\nGot\n%#v", tt.given, tt.expected, actual)
			}
		})
	}
}

func TestCheckResultDetail(t *testing.T) {
	tests := []struct {
		name     string
		given    CheckResult
		expected string
	}{
		{"pass", CheckPass{}, ""},
		{"fail", CheckFail{"Hello"}, "Hello"},
		{"error", CheckError{"Hello"}, "Hello"},
		{"fail 1", CheckFail{"GoodBye"}, "GoodBye"},
		{"error 1", CheckError{"GoodBye"}, "GoodBye"},
	}
	for _, tt := range tests {
		tt := tt
		t.Run(tt.name, func(t *testing.T) {
			actual := tt.given.Detail()
			if actual != tt.expected {
				t.Errorf("\nGiven\n%#v\n\nExpected\n%#v\n\nGot\n%#v", tt.given, tt.expected, actual)
			}

		})
	}
}
