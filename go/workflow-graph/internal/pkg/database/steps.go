package database

import (
	"brook/internal/pkg/workflow"
	"errors"
)

var previousId = 0

const (
	SimpleTranscodeWorkflowId = iota + 1
	InvalidDoubleRootWorkflowId
	InvalidNoStepsWorkflowId
	InvalidNoRootWorkflowId
)

var databaseStepRows = []workflow.Step{}
var databaseLinkRows = []workflow.Link{}

func init() {
	ResetDatabase()
}

func ResetDatabase() {
	// A simple OK workflow
	s1 := insertStep(workflow.StepCheckSize{
		Id:         nextId(),
		WorkflowId: SimpleTranscodeWorkflowId,
	})
	s2 := insertStep(workflow.StepTranscode{
		Id:         nextId(),
		WorkflowId: SimpleTranscodeWorkflowId,
	})
	insertLink(s1, s2)

	// A workflow that is invalid because it has no root (steps with no parent)
	s3 := insertStep(workflow.StepTranscode{
		Id:         nextId(),
		WorkflowId: InvalidNoRootWorkflowId,
	})
	insertLink(s3, s3) // it is its own parent so there is no root

	// A workflow that is invalid because it has multiple roots (steps with no parent)
	insertStep(workflow.StepTranscode{
		Id:         nextId(),
		WorkflowId: InvalidDoubleRootWorkflowId,
	})
	insertStep(workflow.StepTranscode{
		Id:         nextId(),
		WorkflowId: InvalidDoubleRootWorkflowId,
	})
}

func insertStep(step workflow.Step) workflow.Step {
	databaseStepRows = append(databaseStepRows, step)
	return step
}

func insertLink(parent, child workflow.Step) {
	databaseLinkRows = append(databaseLinkRows, workflow.Link{
		ParentId: parent.GetId(),
		ChildId:  child.GetId(),
	})
}

func GetWorkflowRoot(workflowId int) (*workflow.Step, error) {
	roots := []workflow.Step{}

	// Find a step that is doesn't have any links for which it is the child
step:
	for _, step := range databaseStepRows {
		if step.GetWorkflowId() == workflowId {
			for _, link := range databaseLinkRows {
				if link.ChildId == step.GetId() {
					continue step
				}
			}
			roots = append(roots, step)
		}
	}

	if len(roots) == 0 {
		return nil, errors.New("No root found for workflow")
	}

	if len(roots) > 1 {
		return nil, errors.New("Found multiple roots for workflow")
	}

	return &roots[0], nil
}

func nextId() int {
	previousId++
	return previousId
}

func pointerInt(i int) *int {
	return &i
}
