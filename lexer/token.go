package lexer

type TokenType int

const (
	EOF TokenType = iota
	INT
)

type Token struct {
	Type    TokenType
	Literal string
}
