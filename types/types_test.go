package types

import (
	"testing"

	"github.com/alexisbouchez/lingua/parser"
)

func TestTypeCheckerBasic(t *testing.T) {
	src := `
		fn add(a: i32, b: i32): i32 {
			a + b
		}
	`
	p := parser.New(src)
	f := p.ParseFile()

	checker := NewChecker()
	errors := checker.Check(f)

	if len(errors) != 0 {
		t.Errorf("expected no errors, got %v", errors)
	}
}

func TestTypeCheckerTypeMismatch(t *testing.T) {
	src := `
		fn bad(): i32 {
			let x: i32 = 5
			let y: i64 = x
			y
		}
	`
	p := parser.New(src)
	f := p.ParseFile()

	checker := NewChecker()
	errors := checker.Check(f)

	if len(errors) == 0 {
		t.Error("expected type mismatch error")
	}
}

func TestTypeCheckerUndefinedVariable(t *testing.T) {
	src := `
		fn bad(): i32 {
			x
		}
	`
	p := parser.New(src)
	f := p.ParseFile()

	checker := NewChecker()
	errors := checker.Check(f)

	if len(errors) == 0 {
		t.Error("expected undefined variable error")
	}
}

func TestTypeCheckerFunctionCall(t *testing.T) {
	src := `
		fn double(x: i32): i32 { x + x }
		fn main(): i32 {
			double(5)
		}
	`
	p := parser.New(src)
	f := p.ParseFile()

	checker := NewChecker()
	errors := checker.Check(f)

	if len(errors) != 0 {
		t.Errorf("expected no errors, got %v", errors)
	}
}

func TestTypeCheckerWrongArgCount(t *testing.T) {
	src := `
		fn add(a: i32, b: i32): i32 { a + b }
		fn main(): i32 {
			add(5)
		}
	`
	p := parser.New(src)
	f := p.ParseFile()

	checker := NewChecker()
	errors := checker.Check(f)

	if len(errors) == 0 {
		t.Error("expected wrong argument count error")
	}
}

func TestTypeCheckerIfExpr(t *testing.T) {
	src := `
		fn test(x: i32): i32 {
			if x > 0 {
				1
			} else {
				0
			}
		}
	`
	p := parser.New(src)
	f := p.ParseFile()

	checker := NewChecker()
	errors := checker.Check(f)

	if len(errors) != 0 {
		t.Errorf("expected no errors, got %v", errors)
	}
}

func TestTypeCheckerBuiltins(t *testing.T) {
	src := `
		fn test(): i32 {
			let x: i32 = abs(-5)
			let y: i32 = min(1, 2)
			let z: i32 = max(3, 4)
			x + y + z
		}
	`
	p := parser.New(src)
	f := p.ParseFile()

	checker := NewChecker()
	errors := checker.Check(f)

	if len(errors) != 0 {
		t.Errorf("expected no errors, got %v", errors)
	}
}

func TestTypeCheckerGlobals(t *testing.T) {
	src := `
		global counter: i32 = 0
		fn increment(): i32 {
			counter = counter + 1
			counter
		}
	`
	p := parser.New(src)
	f := p.ParseFile()

	checker := NewChecker()
	errors := checker.Check(f)

	if len(errors) != 0 {
		t.Errorf("expected no errors, got %v", errors)
	}
}

func TestTypeCheckerReturnTypeMismatch(t *testing.T) {
	src := `
		fn bad(): i64 {
			42
		}
	`
	p := parser.New(src)
	f := p.ParseFile()

	checker := NewChecker()
	errors := checker.Check(f)

	if len(errors) == 0 {
		t.Error("expected return type mismatch error")
	}
}
