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
)

type Token struct {
	Type    TokenType
	Literal string
}
