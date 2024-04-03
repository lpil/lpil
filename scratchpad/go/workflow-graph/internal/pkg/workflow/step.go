package workflow

type Step interface {
	Visit(StepVisitor) error
}

type StepVisitor interface {
	VisitTranscode(StepTranscode) error
	VisitCheckSize(StepCheckSize) error
}
