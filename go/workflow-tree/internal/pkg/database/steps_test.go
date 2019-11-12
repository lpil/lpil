package database

import (
	"brook/internal/pkg/workflow"
	"errors"
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestGetWorkflowRoot_NoStep(t *testing.T) {
	root, err := GetWorkflowRoot(InvalidNoStepsWorkflowId)
	assert.Nil(t, root)
	assert.Equal(t, errors.New("No root found for workflow"), err)
}

func TestGetWorkflowRoot_NoRoot(t *testing.T) {
	root, err := GetWorkflowRoot(InvalidNoRootWorkflowId)
	assert.Nil(t, root)
	assert.Equal(t, errors.New("No root found for workflow"), err)
}

func TestGetWorkflowRoot_MultipleRoots(t *testing.T) {
	root, err := GetWorkflowRoot(InvalidDoubleRootWorkflowId)
	assert.Nil(t, root)
	assert.Equal(t, errors.New("Found multiple roots for workflow"), err)
}

func TestGetWorkflowRoot_Ok(t *testing.T) {
	root, err := GetWorkflowRoot(SimpleTranscodeWorkflowId)
	assert.Nil(t, err)
	assert.Equal(t, workflow.StepCheckSize{
		Id:           1,
		WorkflowId:   SimpleTranscodeWorkflowId,
		ParentStepId: nil,
	}, *root)
}
