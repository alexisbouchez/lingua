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
	switch p.cur.Type {
	case lexer.INT:
		val, _ := strconv.ParseInt(p.cur.Literal, 10, 64)
		p.next()
		return &IntLit{Value: val}
	case lexer.IDENT:
		name := p.cur.Literal
		p.next()
		return &Ident{Name: name}
	}
	return nil
}

func (p *Parser) expect(t lexer.TokenType) {
	if p.cur.Type != t {
		return
	}
	p.next()
}

func (p *Parser) ParseFn() *FnDecl {
	p.expect(lexer.FN)

	name := p.cur.Literal
	p.next()

	p.expect(lexer.LPAREN)
	params := p.parseParams()
	p.expect(lexer.RPAREN)

	var ret string
	if p.cur.Type == lexer.COLON {
		p.next()
		ret = p.cur.Literal
		p.next()
	}

	body := p.parseBlock()

	return &FnDecl{Name: name, Params: params, Return: ret, Body: body}
}

func (p *Parser) parseBlock() *Block {
	p.expect(lexer.LBRACE)

	var stmts []Stmt
	var finalExpr Expr

	for p.cur.Type != lexer.RBRACE && p.cur.Type != lexer.EOF {
		if p.cur.Type == lexer.LET {
			stmts = append(stmts, p.parseLetStmt())
		} else {
			expr := p.ParseExpr()
			if p.cur.Type == lexer.SEMI {
				p.next()
				stmts = append(stmts, &ExprStmt{Expr: expr})
			} else {
				finalExpr = expr
				break
			}
		}
	}

	p.expect(lexer.RBRACE)
	return &Block{Stmts: stmts, Expr: finalExpr}
}

func (p *Parser) parseLetStmt() *LetStmt {
	p.expect(lexer.LET)
	name := p.cur.Literal
	p.next()

	var typ string
	if p.cur.Type == lexer.COLON {
		p.next()
		typ = p.cur.Literal
		p.next()
	}

	p.expect(lexer.ASSIGN)
	value := p.ParseExpr()
	p.expect(lexer.SEMI)

	return &LetStmt{Name: name, Type: typ, Value: value}
}

func (p *Parser) parseParams() []Param {
	var params []Param
	for p.cur.Type == lexer.IDENT {
		name := p.cur.Literal
		p.next()
		p.expect(lexer.COLON)
		typ := p.cur.Literal
		p.next()
		params = append(params, Param{Name: name, Type: typ})
		if p.cur.Type == lexer.COMMA {
			p.next()
		} else {
			break
		}
	}
	return params
}
