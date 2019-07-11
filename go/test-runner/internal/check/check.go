package check

import (
	"fmt"
)

type Check interface {
	Name() string
	Description() string
	Exec() CheckResult
}

// Running multiple checks
// A reporter callback function is used to allow the caller to inject some
// functionality to be undertaken immediately as each check ends (for example,
// logging and metrics recording).

func RunConcurrently(checks []Check, reporter func(CheckResult, Check)) []CheckResult {
	type indexedCheckResult struct {
		i      int
		result CheckResult
	}

	channel := make(chan indexedCheckResult)
	results := make([]CheckResult, len(checks))

	// Fan out, running all checks at once
	//
	performCheck := func(i int, check Check) {
		result := check.Exec()
		channel <- indexedCheckResult{i, result}
		reporter(result, check)
	}
	for i, check := range checks {
		go performCheck(i, check)
	}

	// Fan in, collecting results from each check
	//
	for range checks {
		r := <-channel
		results[r.i] = r.result
	}

	// We have N checks and have received N results so we can safely close the channel
	close(channel)

	return results
}

// A logger that prints a line for a given check result.
//
func LogResult(result CheckResult, check Check) {
	fmt.Printf("[%s] %s\n", result.String(), check.Name())
}
