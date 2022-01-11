package main

import "fmt"

// Lets use the visitor pattern to simulate a sum type

type Operator interface {
	Visit(v OperatorVisitor)
}

type Add struct{}
type Sub struct{}
type Mul struct{}

// An interface is used for the visitor object to provide more type
// safety than a struct of functions (which could be nil).

type OperatorVisitor interface {
	VisitAdd(op Add)
	VisitSub(op Sub)
	VisitMul(op Mul)
}

func (op Add) Visit(v OperatorVisitor) { v.VisitAdd(op) }
func (op Sub) Visit(v OperatorVisitor) { v.VisitSub(op) }
func (op Mul) Visit(v OperatorVisitor) { v.VisitMul(op) }

// Printing

type PrintOperatorVisitor struct {
	s string
}

// We want to mutate the visitor object in place (so we can have state
// even though the interface has no return value) so a _pointer_ to the
// struct is what implements the interface.

func (v *PrintOperatorVisitor) VisitAdd(_ Add) { v.s = "+" }
func (v *PrintOperatorVisitor) VisitSub(_ Sub) { v.s = "-" }
func (v *PrintOperatorVisitor) VisitMul(_ Mul) { v.s = "/" }

func PrintOperator(op Operator) string {
	v := PrintOperatorVisitor{}
	op.Visit(&v)
	return v.s
}

// Application

type ApplyOperatorVisitor struct {
	ints   []int
	result int
}

func (v *ApplyOperatorVisitor) VisitAdd(_ Add) {
	v.result = 0
	for _, x := range v.ints {
		v.result += x
	}
}
func (v *ApplyOperatorVisitor) VisitSub(_ Sub) {
	v.result = 0
	for _, x := range v.ints {
		v.result -= x
	}
}
func (v *ApplyOperatorVisitor) VisitMul(_ Mul) {
	v.result = 1
	for _, x := range v.ints {
		v.result *= x
	}
}

func ApplyOperator(op Operator, ints []int) int {
	v := ApplyOperatorVisitor{ints: ints}
	op.Visit(&v)
	return v.result
}

type Container struct {
	item int
}

func update(c Container) {
	fmt.Println("Setting container.item to 100")
	c.item = 100
}

func main() {
	fmt.Println(PrintOperator(Add{}))
	fmt.Println(PrintOperator(Sub{}))
	fmt.Println(PrintOperator(Mul{}))

	fmt.Printf("%d\n", ApplyOperator(Add{}, []int{1, 2, 3, 4}))
	fmt.Printf("%d\n", ApplyOperator(Sub{}, []int{1, 2, 3, 4}))
	fmt.Printf("%d\n", ApplyOperator(Mul{}, []int{1, 2, 3, 4}))
}
