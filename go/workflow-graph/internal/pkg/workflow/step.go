package workflow

type Step interface {
	GetId() int
	GetWorkflowId() int
	Visit(StepVisitor) error
}

type StepVisitor interface {
	VisitTranscode(StepTranscode) error
	VisitCheckSize(StepCheckSize) error
}
