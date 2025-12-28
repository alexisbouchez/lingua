package parser

import "testing"

func TestParseIntLit(t *testing.T) {
	p := New("42")
	expr := p.ParseExpr()
	lit, ok := expr.(*IntLit)
	if !ok {
		t.Fatalf("expected *IntLit, got %T", expr)
	}
	if lit.Value != 42 {
		t.Fatalf("expected 42, got %d", lit.Value)
	}
}

func TestParseBinaryExpr(t *testing.T) {
	p := New("1 + 2")
	expr := p.ParseExpr()
	bin, ok := expr.(*BinaryExpr)
	if !ok {
		t.Fatalf("expected *BinaryExpr, got %T", expr)
	}
	if bin.Op != "+" {
		t.Fatalf("expected +, got %s", bin.Op)
	}

	left, ok := bin.Left.(*IntLit)
	if !ok || left.Value != 1 {
		t.Fatalf("expected left=1, got %v", bin.Left)
	}

	right, ok := bin.Right.(*IntLit)
	if !ok || right.Value != 2 {
		t.Fatalf("expected right=2, got %v", bin.Right)
	}
}

func TestParsePrecedence(t *testing.T) {
	// 1 + 2 * 3 should parse as 1 + (2 * 3)
	p := New("1 + 2 * 3")
	expr := p.ParseExpr()
	bin, ok := expr.(*BinaryExpr)
	if !ok {
		t.Fatalf("expected *BinaryExpr, got %T", expr)
	}
	if bin.Op != "+" {
		t.Fatalf("expected top op +, got %s", bin.Op)
	}

	left, ok := bin.Left.(*IntLit)
	if !ok || left.Value != 1 {
		t.Fatalf("expected left=1")
	}

	right, ok := bin.Right.(*BinaryExpr)
	if !ok || right.Op != "*" {
		t.Fatalf("expected right to be 2*3")
	}
}

func TestParseFnDecl(t *testing.T) {
	p := New("fn add(a: i32, b: i32): i32 { a + b }")
	fn := p.ParseFn()
	if fn.Name != "add" {
		t.Fatalf("expected name=add, got %s", fn.Name)
	}
	if len(fn.Params) != 2 {
		t.Fatalf("expected 2 params, got %d", len(fn.Params))
	}
	if fn.Params[0].Name != "a" || fn.Params[0].Type != "i32" {
		t.Fatalf("wrong param 0: %+v", fn.Params[0])
	}
	if fn.Return != "i32" {
		t.Fatalf("expected return=i32, got %s", fn.Return)
	}
	if fn.Body == nil {
		t.Fatalf("expected body")
	}
}

func TestParseLetStmt(t *testing.T) {
	p := New("fn foo(x: i32): i32 { let y: i32 = x * 2; y + 1 }")
	fn := p.ParseFn()
	if fn.Name != "foo" {
		t.Fatalf("expected foo, got %s", fn.Name)
	}
	if len(fn.Body.Stmts) != 1 {
		t.Fatalf("expected 1 stmt, got %d", len(fn.Body.Stmts))
	}
	let, ok := fn.Body.Stmts[0].(*LetStmt)
	if !ok {
		t.Fatalf("expected LetStmt, got %T", fn.Body.Stmts[0])
	}
	if let.Name != "y" {
		t.Fatalf("expected y, got %s", let.Name)
	}
	if fn.Body.Expr == nil {
		t.Fatalf("expected final expr")
	}
}
