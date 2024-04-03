package workflow

import (
	"brook/internal/pkg/transcode"
	"math/rand"
	"time"
)

// A workflow step visitor that runs the given step

type VisitorExec struct {
	TranscodeService transcode.Service
}

func (visitor VisitorExec) VisitCheckSize(step StepCheckSize) error {
	// TODO: Need to get the input file path from the workflow
	return step.CheckVideoSize("input file path")
}

func (visitor VisitorExec) VisitTranscode(step StepTranscode) error {
	// TODO: Need to get the input file path and config from somewhere
	if err := step.StartTranscoding(visitor.TranscodeService); err != nil {
		return err
	}

	// TODO: do this async (in a goroutine) to simulate a webhook coming
	// back into the platform.
	time.Sleep(time.Duration(rand.Intn(1000)) * time.Millisecond)

	return step.HandleTranscodingFinish()
}
