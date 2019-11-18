package database

import (
	"brook/internal/pkg/workflow"
)

const (
	SimpleTranscodeWorkflowId = iota + 1
	InvalidDoubleRootWorkflowId
	InvalidNoStepsWorkflowId
	InvalidNoRootWorkflowId
)

var databaseStepRows = []Step{}
var databaseLinkRows = []Link{}

func init() {
	ResetDatabase()
}

func ResetDatabase() {
	// A simple OK workflow
	s1 := insertStep(SimpleTranscodeWorkflowId, workflow.StepCheckSize{})
	s2 := insertStep(SimpleTranscodeWorkflowId, workflow.StepTranscode{})
	insertLink(s1, s2)

	// A workflow that is invalid because it has no root (steps with no parent)
	s3 := insertStep(InvalidNoRootWorkflowId, workflow.StepTranscode{})
	insertLink(s3, s3) // it is its own parent so there is no root

	// A workflow that is invalid because it has multiple roots
	// (steps with no parent)
	insertStep(InvalidDoubleRootWorkflowId, workflow.StepTranscode{})
	insertStep(InvalidDoubleRootWorkflowId, workflow.StepTranscode{})
}

func insertStep(workflowId int, step workflow.Step) Step {
	s := NewStep(workflowId, step)
	databaseStepRows = append(databaseStepRows, s)
	return s
}

func insertLink(parent, child Step) {
	databaseLinkRows = append(databaseLinkRows, Link{
		ParentId: parent.Id,
		ChildId:  child.Id,
	})
}
