package parser

type Node interface {
	node()
}

type Expr interface {
	Node
	expr()
}

type IntLit struct {
	Value int64
}

func (IntLit) node() {}
func (IntLit) expr() {}

type BinaryExpr struct {
	Left  Expr
	Op    string
	Right Expr
}

func (BinaryExpr) node() {}
func (BinaryExpr) expr() {}
