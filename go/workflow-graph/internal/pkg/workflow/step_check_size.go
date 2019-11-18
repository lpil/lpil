package workflow

type StepCheckSize struct {
	Id           int
	WorkflowId   int
	ParentStepId *int
}

// StepCheckSize implements the Step interface

func (step StepCheckSize) GetId() int {
	return step.Id
}

func (step StepCheckSize) GetWorkflowId() int {
	return step.WorkflowId
}

func (step StepCheckSize) GetParentStepId() *int {
	return step.ParentStepId
}

func (step StepCheckSize) Visit(visitor StepVisitor) error {
	return visitor.VisitCheckSize(step)
}

// StepCheckSize can be ran to check the size of a video

func (step StepCheckSize) CheckVideoSize(inputFilePath string) error {
	return nil
}
