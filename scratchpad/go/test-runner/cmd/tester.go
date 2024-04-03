package main

import (
	"github.com/lpil/learning/go/test-runner/internal/check"
	log "github.com/sirupsen/logrus"
)

func main() {
	logger := log.WithFields(log.Fields{
		"in": "main",
	})

	logger.WithFields(log.Fields{"what": "started"}).Info("Started")

	checks := []check.Check{
		check.One{},
		check.One{},
		check.One{},
	}

	check.RunConcurrently(checks, check.LogResult)

	logger.WithFields(log.Fields{"what": "finished"}).Info("Finished")
}
