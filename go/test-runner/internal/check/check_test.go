package check

import (
	"reflect"
	"testing"
)

func TestRunConcurrently(t *testing.T) {
	checks := []Check{
		testCheck{true, "ok"},
		testCheck{false, "ok"},
		testCheck{true, ":)"},
	}

	results := RunConcurrently(checks)

	expected := []CheckResult{
		CheckResult{true, "ok"},
		CheckResult{false, "ok"},
		CheckResult{true, ":)"},
	}

	if !reflect.DeepEqual(expected, results) {
		t.Errorf("\n\nExpected\n%#v\n\nGot\n%#v", expected, results)
	}
}

// A test Check that returns the contained values.

type testCheck struct {
	pass        bool
	description string
}

func (_ testCheck) Description() string {
	return ""
}

func (_ testCheck) Name() string {
	return ""
}

func (t testCheck) Exec() CheckResult {
	return CheckResult{t.pass, t.description}
}
