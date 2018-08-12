// There is a more safe version of this visitor pattern in ../interface-visitor
package main

import "fmt"

// Lets use the visitor pattern to simulate a sum type

type Operator interface {
	Visit(v OperatorVisitor)
}

type Add struct{}
type Sub struct{}
type Mul struct{}

type OperatorVisitor struct {
	VisitAdd func(Add)
	VisitSub func(Sub)
	VisitMul func(Mul)
}

func (op Add) Visit(v OperatorVisitor) { v.VisitAdd(op) }
func (op Sub) Visit(v OperatorVisitor) { v.VisitSub(op) }
func (op Mul) Visit(v OperatorVisitor) { v.VisitMul(op) }

func PrintOperator(op Operator) (s string) {
	op.Visit(OperatorVisitor{
		func(_ Add) { s = "+" },
		func(_ Sub) { s = "-" },
		func(_ Mul) { s = "/" },
	})
	return
}

func ApplyOperator(op Operator, ints []int) (i int) {
	op.Visit(OperatorVisitor{
		func(_ Add) {
			i = 0
			for _, v := range ints {
				i += v
			}
		},
		func(_ Sub) {
			i = 0
			for _, v := range ints {
				i -= v
			}
		},
		func(_ Mul) {
			i = 1
			for _, v := range ints {
				i *= v
			}
		},
	})
	return
}

func main() {
	fmt.Println(PrintOperator(Add{}))
	fmt.Println(PrintOperator(Sub{}))
	fmt.Println(PrintOperator(Mul{}))

	fmt.Printf("%d\n", ApplyOperator(Add{}, []int{1, 2, 3, 4}))
	fmt.Printf("%d\n", ApplyOperator(Sub{}, []int{1, 2, 3, 4}))
	fmt.Printf("%d\n", ApplyOperator(Mul{}, []int{1, 2, 3, 4}))
}
