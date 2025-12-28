package lexer

type TokenType int

const (
	EOF TokenType = iota
	INT

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
)

type Token struct {
	Type    TokenType
	Literal string
}
