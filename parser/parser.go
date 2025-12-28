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
	return p.parseExprPrec(0)
}

func (p *Parser) parseExprPrec(minPrec int) Expr {
	left := p.parsePrimary()

	for {
		prec := p.precedence()
		if prec < minPrec {
			break
		}
		op := p.cur.Literal
		p.next()
		right := p.parseExprPrec(prec + 1)
		left = &BinaryExpr{Left: left, Op: op, Right: right}
	}

	return left
}

func (p *Parser) precedence() int {
	switch p.cur.Type {
	case lexer.PLUS, lexer.MINUS:
		return 1
	case lexer.STAR, lexer.SLASH, lexer.PERCENT:
		return 2
	default:
		return -1
	}
}

func (p *Parser) parsePrimary() Expr {
	if p.cur.Type == lexer.INT {
		val, _ := strconv.ParseInt(p.cur.Literal, 10, 64)
		p.next()
		return &IntLit{Value: val}
	}
	return nil
}
