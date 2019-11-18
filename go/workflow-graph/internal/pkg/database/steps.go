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

func init() {
	ResetDatabase()
}

func ResetDatabase() {
	databaseStepRows = []workflow.Step{
		// A simple OK workflow
		workflow.StepCheckSize{
			Id:           nextId(),
			WorkflowId:   SimpleTranscodeWorkflowId,
			ParentStepId: nil,
		},
		workflow.StepTranscode{
			Id:           nextId(),
			WorkflowId:   SimpleTranscodeWorkflowId,
			ParentStepId: pointerInt(previousId),
		},

		// A workflow that is invalid because it has no root (steps with no parent)
		workflow.StepTranscode{
			Id:           nextId(),
			WorkflowId:   InvalidNoRootWorkflowId,
			ParentStepId: pointerInt(0),
		},

		// A workflow that is invalid because it has multiple roots (steps with no parent)
		workflow.StepTranscode{
			Id:           nextId(),
			WorkflowId:   InvalidDoubleRootWorkflowId,
			ParentStepId: nil,
		},
		workflow.StepTranscode{
			Id:           nextId(),
			WorkflowId:   InvalidDoubleRootWorkflowId,
			ParentStepId: nil,
		},
	}
}

func GetWorkflowRoot(workflowId int) (*workflow.Step, error) {
	roots := []workflow.Step{}

	// SELECT * FROM steps WHERE parent_id IS NULL LIMIT 2;
	for _, step := range databaseStepRows {
		if step.GetParentStepId() == nil && step.GetWorkflowId() == workflowId {
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
