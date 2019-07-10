package check

import (
	"reflect"
	"testing"
)

func TestRunConcurrently(t *testing.T) {
	checks := []Check{
		testCheck{CheckPass{}},
		testCheck{CheckFail{"sad"}},
		testCheck{CheckError{"mad"}},
	}

	results := RunConcurrently(checks)

	expected := []CheckResult{
		CheckPass{},
		CheckFail{"sad"},
		CheckError{"mad"},
	}

	if !reflect.DeepEqual(expected, results) {
		t.Errorf("\n\nExpected\n%#v\n\nGot\n%#v", expected, results)
	}
}

// A test Check that returns the contained values.

type testCheck struct {
	result CheckResult
}

func (_ testCheck) Description() string {
	return ""
}

func (_ testCheck) Name() string {
	return ""
}

func (t testCheck) Exec() CheckResult {
	return t.result
}
