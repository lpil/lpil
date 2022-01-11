package workflow

type StepCheckSize struct {
}

// StepCheckSize implements the Step interface

func (step StepCheckSize) Visit(visitor StepVisitor) error {
	return visitor.VisitCheckSize(step)
}

// StepCheckSize can be ran to check the size of a video

func (step StepCheckSize) CheckVideoSize(inputFilePath string) error {
	return nil
}
