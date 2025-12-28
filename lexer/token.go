package lexer

type TokenType int

const (
	EOF TokenType = iota
	INT
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
	COMMA
	SEMI
	COLON

	ASSIGN
	EQ
	NEQ
	LT
	GT
	LTE
	GTE

	// keywords
	FN
	LET
	RETURN
	IF
	ELSE
	LOOP

	// wasm types
	I32
	I64
	F32
	F64
)

var keywords = map[string]TokenType{
	"fn":     FN,
	"let":    LET,
	"return": RETURN,
	"if":     IF,
	"else":   ELSE,
	"loop":   LOOP,
	"i32":    I32,
	"i64":    I64,
	"f32":    F32,
	"f64":    F64,
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
}
