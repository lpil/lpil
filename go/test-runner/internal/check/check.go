package check

type Check interface {
	Name() string
	Description() string
	Exec() CheckResult
}

// Running multiple checks

func RunConcurrently(checks []Check) []CheckResult {
	type indexedCheckResult struct {
		i      int
		result CheckResult
	}

	channel := make(chan indexedCheckResult)
	results := make([]CheckResult, len(checks))

	// Fan out, running all checks at once
	//
	performCheck := func(i int, check Check) {
		channel <- indexedCheckResult{i, check.Exec()}
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
