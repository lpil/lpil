package database

import (
	"brook/internal/pkg/workflow"
	"errors"
)

type Step struct {
	Id         int
	WorkflowId int
	Step       workflow.Step
}

var id = 0

func NewStep(workflowId int, step workflow.Step) Step {
	id++
	return Step{
		Id:         id,
		WorkflowId: workflowId,
		Step:       step,
	}
}

func GetWorkflowRoot(workflowId int) (*Step, error) {
	roots := []Step{}

	// Find a step that is doesn't have any links for which it is the child
step:
	for _, step := range databaseStepRows {
		if step.WorkflowId == workflowId {
			for _, link := range databaseLinkRows {
				if link.ChildId == step.Id {
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
