package workflow

import (
	"brook/internal/pkg/transcode"
)

type StepTranscode struct {
	Id           int
	WorkflowId   int
	ParentStepId *int
}

// StepTranscode implements the Step interface

func (step StepTranscode) GetId() int {
	return step.Id
}

func (step StepTranscode) GetWorkflowId() int {
	return step.WorkflowId
}

func (step StepTranscode) GetParentStepId() *int {
	return step.ParentStepId
}

func (step StepTranscode) Visit(visitor StepVisitor) error {
	return visitor.VisitTranscode(step)
}

// StepTranscode can be ran to transcode a video

func (step StepTranscode) StartTranscoding(transcoder transcode.Service) error {
	// TODO: we need a way of passing in the transcode configuration and
	// the dynamic information into this step (i.e. the file path)

	// TODO: We need to be able to kick off this transcode video step and
	// then shut down, waiting for the result to happen so it can restart
	// again
	_, err := transcoder.TranscodeVideo("input file path", "output file path", "transcode profile")

	// TODO: Do something with the transcoder's returned data here. Need to
	// store it somewhere.

	return err
}

func (step StepTranscode) HandleTranscodingFinish() error {
	// TODO: record output somewhere
	return nil
}
