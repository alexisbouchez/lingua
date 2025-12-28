package lexer

type TokenType int

const (
	EOF TokenType = iota
	INT
	STRING
	IDENT

	PLUS
	MINUS
	STAR
	SLASH
	PERCENT

	LPAREN
	RPAREN
	LBRACE
	RBRACE
	LBRACKET
	RBRACKET
	COMMA
	SEMI
	COLON
	DOT

	ASSIGN
	EQ
	NEQ
	LT
	GT
	LTE
	GTE
	AND
	OR
	NOT
	BAND    // &
	BOR     // |
	BXOR    // ^
	SHL     // <<
	SHR     // >>

	// keywords
	FN
	LET
	GLOBAL
	RETURN
	IF
	ELSE
	LOOP
	BREAK
	CONTINUE
	STRUCT

	// wasm types
	I32
	I64
	F32
	F64
)

var keywords = map[string]TokenType{
	"fn":       FN,
	"let":      LET,
	"global":   GLOBAL,
	"return":   RETURN,
	"if":       IF,
	"else":     ELSE,
	"loop":     LOOP,
	"break":    BREAK,
	"continue": CONTINUE,
	"struct":   STRUCT,
	"i32":      I32,
	"i64":      I64,
	"f32":      F32,
	"f64":      F64,
}

func LookupIdent(ident string) TokenType {
	if tok, ok := keywords[ident]; ok {
		return tok
	}
	return IDENT
}

type Token struct {
	Type    TokenType
	Literal string
	Line    int
	Column  int
}
