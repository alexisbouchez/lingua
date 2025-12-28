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

type Ident struct {
	Name string
}

func (Ident) node() {}
func (Ident) expr() {}

type Param struct {
	Name string
	Type string
}

type Stmt interface {
	Node
	stmt()
}

type LetStmt struct {
	Name  string
	Type  string
	Value Expr
}

func (LetStmt) node() {}
func (LetStmt) stmt() {}

type ExprStmt struct {
	Expr Expr
}

func (ExprStmt) node() {}
func (ExprStmt) stmt() {}

type AssignStmt struct {
	Name  string
	Value Expr
}

func (AssignStmt) node() {}
func (AssignStmt) stmt() {}

type Block struct {
	Stmts []Stmt
	Expr  Expr // final expression (return value)
}

func (Block) node() {}
func (Block) expr() {}

type IfExpr struct {
	Cond Expr
	Then *Block
	Else *Block
}

func (IfExpr) node() {}
func (IfExpr) expr() {}

type LoopExpr struct {
	Cond Expr
	Body *Block
}

func (LoopExpr) node() {}
func (LoopExpr) expr() {}

type FnDecl struct {
	Name   string
	Params []Param
	Return string
	Body   *Block
}

func (FnDecl) node() {}

type CallExpr struct {
	Name string
	Args []Expr
}

func (CallExpr) node() {}
func (CallExpr) expr() {}

type File struct {
	Fns []*FnDecl
}

func (File) node() {}
