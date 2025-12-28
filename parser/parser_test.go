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
