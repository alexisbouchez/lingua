package lexer

import "testing"

func TestNextToken_Empty(t *testing.T) {
	l := New("")
	tok := l.NextToken()
	if tok.Type != EOF {
		t.Fatalf("expected EOF, got %v", tok.Type)
	}
}

func TestNextToken_Integer(t *testing.T) {
	l := New("42")
	tok := l.NextToken()
	if tok.Type != INT {
		t.Fatalf("expected INT, got %v", tok.Type)
	}
	if tok.Literal != "42" {
		t.Fatalf("expected '42', got %q", tok.Literal)
	}
}

func TestNextToken_Operators(t *testing.T) {
	input := "+-*/%"
	tests := []struct {
		typ TokenType
		lit string
	}{
		{PLUS, "+"},
		{MINUS, "-"},
		{STAR, "*"},
		{SLASH, "/"},
		{PERCENT, "%"},
		{EOF, ""},
	}

	l := New(input)
	for i, tt := range tests {
		tok := l.NextToken()
		if tok.Type != tt.typ {
			t.Fatalf("tests[%d] - wrong type. expected=%v, got=%v", i, tt.typ, tok.Type)
		}
		if tok.Literal != tt.lit {
			t.Fatalf("tests[%d] - wrong literal. expected=%q, got=%q", i, tt.lit, tok.Literal)
		}
	}
}
