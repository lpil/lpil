package main

import (
	"fmt"
	"github.com/lpil/learning/go/test-runner/internal/check"
)

func main() {
	checks := []check.Check{
		check.One{},
		check.One{},
		check.One{},
	}

	results := check.RunConcurrently(checks, check.LogResult)

	fmt.Printf("%#v\n", results)
}
