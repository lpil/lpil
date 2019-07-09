package check

type Check interface {
	Name() string
	Description() string
	Exec() CheckResult
}

type CheckResult struct {
	Pass   bool
	Detail string
}

func pass() CheckResult {
	return CheckResult{true, ""}
}

func fail(detail string) CheckResult {
	return CheckResult{false, detail}
}

func RunConcurrently(checks []Check) []CheckResult {
	type indexedCheckResult struct {
		i      int
		result CheckResult
	}

	channel := make(chan indexedCheckResult)
	results := make([]CheckResult, len(checks))

	// Fan out, spawning a goroutine per check
	//
	for i, check := range checks {
		go func(i int, check Check) {
			channel <- indexedCheckResult{i, check.Exec()}
		}(i, check)
	}

	// Fan in, collecting results from each check
	//
	for range checks {
		r := <-channel
		results[r.i] = r.result
	}

	return results

}
