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
