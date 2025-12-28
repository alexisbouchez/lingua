package parser

import (
	"strconv"

	"github.com/alexisbouchez/lingua/lexer"
)

type Parser struct {
	l   *lexer.Lexer
	cur lexer.Token
}

func New(input string) *Parser {
	p := &Parser{l: lexer.New(input)}
	p.next()
	return p
}

func (p *Parser) next() {
	p.cur = p.l.NextToken()
}

func (p *Parser) ParseExpr() Expr {
	left := p.parsePrimary()

	for p.cur.Type == lexer.PLUS || p.cur.Type == lexer.MINUS ||
		p.cur.Type == lexer.STAR || p.cur.Type == lexer.SLASH {
		op := p.cur.Literal
		p.next()
		right := p.parsePrimary()
		left = &BinaryExpr{Left: left, Op: op, Right: right}
	}

	return left
}

func (p *Parser) parsePrimary() Expr {
	if p.cur.Type == lexer.INT {
		val, _ := strconv.ParseInt(p.cur.Literal, 10, 64)
		p.next()
		return &IntLit{Value: val}
	}
	return nil
}
