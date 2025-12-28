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

type StringLit struct {
	Value string
}

func (StringLit) node() {}
func (StringLit) expr() {}

type BinaryExpr struct {
	Left  Expr
	Op    string
	Right Expr
}

func (BinaryExpr) node() {}
func (BinaryExpr) expr() {}

type UnaryExpr struct {
	Op   string
	Expr Expr
}

func (UnaryExpr) node() {}
func (UnaryExpr) expr() {}

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

type IndexAssignStmt struct {
	Array Expr
	Index Expr
	Value Expr
}

func (IndexAssignStmt) node() {}
func (IndexAssignStmt) stmt() {}

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

type BreakExpr struct{}

func (BreakExpr) node() {}
func (BreakExpr) expr() {}

type ContinueExpr struct{}

func (ContinueExpr) node() {}
func (ContinueExpr) expr() {}

type ReturnExpr struct {
	Value Expr
}

func (ReturnExpr) node() {}
func (ReturnExpr) expr() {}

type FnDecl struct {
	Name   string
	Params []Param
	Return string
	Body   *Block
}

func (FnDecl) node() {}

type GlobalDecl struct {
	Name  string
	Type  string
	Value Expr
}

func (GlobalDecl) node() {}

type CallExpr struct {
	Name string
	Args []Expr
}

func (CallExpr) node() {}
func (CallExpr) expr() {}

type ArrayLit struct {
	Elements []Expr
}

func (ArrayLit) node() {}
func (ArrayLit) expr() {}

type IndexExpr struct {
	Array Expr
	Index Expr
}

func (IndexExpr) node() {}
func (IndexExpr) expr() {}

type File struct {
	Globals []*GlobalDecl
	Fns     []*FnDecl
}

func (File) node() {}
