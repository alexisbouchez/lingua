package parser

import (
	"strconv"

	"github.com/alexisbouchez/lingua/errors"
	"github.com/alexisbouchez/lingua/lexer"
)

type Parser struct {
	l      *lexer.Lexer
	cur    lexer.Token
	errors *errors.ErrorList
	source string
}

func New(input string) *Parser {
	errList := errors.NewErrorList()
	errList.Source = input
	p := &Parser{
		l:      lexer.New(input),
		errors: errList,
		source: input,
	}
	p.next()
	return p
}

func (p *Parser) Errors() *errors.ErrorList {
	return p.errors
}

func (p *Parser) next() {
	p.cur = p.l.NextToken()
}

// isStructLitStart checks if current LBRACE starts a struct literal.
// Struct literals look like { field: value, ... } so after { we should see IDENT COLON.
// If we see just an expression (like { a } for if blocks), it's not a struct literal.
func (p *Parser) isStructLitStart() bool {
	// Create a new lexer at the same position to peek ahead
	// Current token is LBRACE, we need to see what's after it
	peekLexer := lexer.New(p.source)
	// Advance to current position by reading tokens until we reach current line/column
	for {
		tok := peekLexer.NextToken()
		if tok.Type == lexer.EOF {
			return false
		}
		if tok.Type == lexer.LBRACE && tok.Line == p.cur.Line && tok.Column == p.cur.Column {
			break
		}
	}
	// Now peek the next two tokens
	tok1 := peekLexer.NextToken()
	if tok1.Type != lexer.IDENT {
		return false
	}
	tok2 := peekLexer.NextToken()
	return tok2.Type == lexer.COLON
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
	case lexer.OR:
		return 0
	case lexer.AND:
		return 1
	case lexer.BOR:
		return 2
	case lexer.BXOR:
		return 3
	case lexer.BAND:
		return 4
	case lexer.EQ, lexer.NEQ, lexer.LT, lexer.GT, lexer.LTE, lexer.GTE:
		return 5
	case lexer.SHL, lexer.SHR:
		return 6
	case lexer.PLUS, lexer.MINUS:
		return 7
	case lexer.STAR, lexer.SLASH, lexer.PERCENT:
		return 8
	default:
		return -1
	}
}

func (p *Parser) parsePrimary() Expr {
	switch p.cur.Type {
	case lexer.NOT:
		p.next()
		return &UnaryExpr{Op: "!", Expr: p.parsePrimary()}
	case lexer.MINUS:
		p.next()
		return &UnaryExpr{Op: "-", Expr: p.parsePrimary()}
	case lexer.INT:
		val, _ := strconv.ParseInt(p.cur.Literal, 10, 64)
		p.next()
		return &IntLit{Value: val}
	case lexer.STRING:
		val := p.cur.Literal
		p.next()
		return &StringLit{Value: val}
	case lexer.IDENT:
		name := p.cur.Literal
		p.next()
		if p.cur.Type == lexer.LPAREN {
			return p.parseCallExpr(name)
		}
		// Check for struct literal: StructName { field: value, ... }
		// Must look ahead to distinguish from blocks: if b { x } vs Point { x: 1 }
		if p.cur.Type == lexer.LBRACE && p.isStructLitStart() {
			return p.parseStructLit(name)
		}
		var expr Expr = &Ident{Name: name}
		// Handle index expressions and field access
		for p.cur.Type == lexer.LBRACKET || p.cur.Type == lexer.DOT {
			if p.cur.Type == lexer.LBRACKET {
				p.next()
				index := p.ParseExpr()
				p.expect(lexer.RBRACKET)
				expr = &IndexExpr{Array: expr, Index: index}
			} else if p.cur.Type == lexer.DOT {
				p.next()
				field := p.cur.Literal
				p.next()
				expr = &FieldExpr{Expr: expr, Field: field}
			}
		}
		return expr
	case lexer.IF:
		return p.parseIfExpr()
	case lexer.LOOP:
		return p.parseLoopExpr()
	case lexer.BREAK:
		p.next()
		return &BreakExpr{}
	case lexer.CONTINUE:
		p.next()
		return &ContinueExpr{}
	case lexer.RETURN:
		p.next()
		return &ReturnExpr{Value: p.ParseExpr()}
	case lexer.AWAIT:
		p.next()
		return &AwaitExpr{Expr: p.ParseExpr()}
	case lexer.LBRACKET:
		var expr Expr = p.parseArrayLit()
		// Handle index expressions: [1,2,3][0]
		for p.cur.Type == lexer.LBRACKET {
			p.next()
			index := p.ParseExpr()
			p.expect(lexer.RBRACKET)
			expr = &IndexExpr{Array: expr, Index: index}
		}
		return expr
	}
	return nil
}

func (p *Parser) parseLoopExpr() *LoopExpr {
	p.expect(lexer.LOOP)
	cond := p.ParseExpr()
	body := p.parseBlock()
	return &LoopExpr{Cond: cond, Body: body}
}

func (p *Parser) parseIfExpr() *IfExpr {
	p.expect(lexer.IF)
	cond := p.ParseExpr()
	then := p.parseBlock()
	var els *Block
	if p.cur.Type == lexer.ELSE {
		p.next()
		els = p.parseBlock()
	}
	return &IfExpr{Cond: cond, Then: then, Else: els}
}

func (p *Parser) expect(t lexer.TokenType) {
	if p.cur.Type != t {
		p.errors.Add(
			"expected "+tokenTypeName(t)+", got "+tokenTypeName(p.cur.Type),
			p.cur.Line,
			p.cur.Column,
		)
		return
	}
	p.next()
}

func tokenTypeName(t lexer.TokenType) string {
	switch t {
	case lexer.EOF:
		return "EOF"
	case lexer.INT:
		return "integer"
	case lexer.STRING:
		return "string"
	case lexer.IDENT:
		return "identifier"
	case lexer.PLUS:
		return "'+'"
	case lexer.MINUS:
		return "'-'"
	case lexer.STAR:
		return "'*'"
	case lexer.SLASH:
		return "'/'"
	case lexer.PERCENT:
		return "'%'"
	case lexer.LPAREN:
		return "'('"
	case lexer.RPAREN:
		return "')'"
	case lexer.LBRACE:
		return "'{'"
	case lexer.RBRACE:
		return "'}'"
	case lexer.LBRACKET:
		return "'['"
	case lexer.RBRACKET:
		return "']'"
	case lexer.COMMA:
		return "','"
	case lexer.SEMI:
		return "';'"
	case lexer.COLON:
		return "':'"
	case lexer.DOT:
		return "'.'"
	case lexer.ASSIGN:
		return "'='"
	case lexer.EQ:
		return "'=='"
	case lexer.NEQ:
		return "'!='"
	case lexer.LT:
		return "'<'"
	case lexer.GT:
		return "'>'"
	case lexer.LTE:
		return "'<='"
	case lexer.GTE:
		return "'>='"
	case lexer.AND:
		return "'&&'"
	case lexer.OR:
		return "'||'"
	case lexer.NOT:
		return "'!'"
	case lexer.FN:
		return "'fn'"
	case lexer.LET:
		return "'let'"
	case lexer.GLOBAL:
		return "'global'"
	case lexer.RETURN:
		return "'return'"
	case lexer.IF:
		return "'if'"
	case lexer.ELSE:
		return "'else'"
	case lexer.LOOP:
		return "'loop'"
	case lexer.BREAK:
		return "'break'"
	case lexer.CONTINUE:
		return "'continue'"
	case lexer.STRUCT:
		return "'struct'"
	case lexer.I32:
		return "'i32'"
	case lexer.I64:
		return "'i64'"
	case lexer.F32:
		return "'f32'"
	case lexer.F64:
		return "'f64'"
	default:
		return "unknown token"
	}
}

func (p *Parser) ParseFn() *FnDecl {
	isAsync := false
	if p.cur.Type == lexer.ASYNC {
		isAsync = true
		p.next()
	}
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

	return &FnDecl{Name: name, Params: params, Return: ret, Body: body, Async: isAsync}
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
			// Check for assignment: ident = expr; or arr[i] = expr; or struct.field = expr;
			if p.cur.Type == lexer.ASSIGN {
				if ident, ok := expr.(*Ident); ok {
					p.next()
					value := p.ParseExpr()
					p.expect(lexer.SEMI)
					stmts = append(stmts, &AssignStmt{Name: ident.Name, Value: value})
					continue
				}
				if idx, ok := expr.(*IndexExpr); ok {
					p.next()
					value := p.ParseExpr()
					p.expect(lexer.SEMI)
					stmts = append(stmts, &IndexAssignStmt{Array: idx.Array, Index: idx.Index, Value: value})
					continue
				}
				if field, ok := expr.(*FieldExpr); ok {
					p.next()
					value := p.ParseExpr()
					p.expect(lexer.SEMI)
					stmts = append(stmts, &FieldAssignStmt{Expr: field.Expr, Field: field.Field, Value: value})
					continue
				}
			}
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

func (p *Parser) parseCallExpr(name string) *CallExpr {
	p.expect(lexer.LPAREN)
	var args []Expr
	for p.cur.Type != lexer.RPAREN && p.cur.Type != lexer.EOF {
		args = append(args, p.ParseExpr())
		if p.cur.Type == lexer.COMMA {
			p.next()
		} else {
			break
		}
	}
	p.expect(lexer.RPAREN)
	return &CallExpr{Name: name, Args: args}
}

func (p *Parser) parseArrayLit() *ArrayLit {
	p.expect(lexer.LBRACKET)
	var elements []Expr
	for p.cur.Type != lexer.RBRACKET && p.cur.Type != lexer.EOF {
		elements = append(elements, p.ParseExpr())
		if p.cur.Type == lexer.COMMA {
			p.next()
		} else {
			break
		}
	}
	p.expect(lexer.RBRACKET)
	return &ArrayLit{Elements: elements}
}

func (p *Parser) parseStructLit(name string) *StructLit {
	p.expect(lexer.LBRACE)
	var fields []StructFieldInit
	for p.cur.Type == lexer.IDENT {
		fieldName := p.cur.Literal
		p.next()
		p.expect(lexer.COLON)
		value := p.ParseExpr()
		fields = append(fields, StructFieldInit{Name: fieldName, Value: value})
		if p.cur.Type == lexer.COMMA {
			p.next()
		} else {
			break
		}
	}
	p.expect(lexer.RBRACE)
	return &StructLit{Name: name, Fields: fields}
}

func (p *Parser) ParseFile() *File {
	var structs []*StructDecl
	var globals []*GlobalDecl
	var fns []*FnDecl
	for p.cur.Type != lexer.EOF {
		if p.cur.Type == lexer.STRUCT {
			structs = append(structs, p.parseStruct())
		} else if p.cur.Type == lexer.GLOBAL {
			globals = append(globals, p.parseGlobal())
		} else {
			fns = append(fns, p.ParseFn())
		}
	}
	return &File{Structs: structs, Globals: globals, Fns: fns}
}

func (p *Parser) parseStruct() *StructDecl {
	p.expect(lexer.STRUCT)
	name := p.cur.Literal
	p.next()
	p.expect(lexer.LBRACE)

	var fields []StructField
	for p.cur.Type == lexer.IDENT {
		fieldName := p.cur.Literal
		p.next()
		p.expect(lexer.COLON)
		fieldType := p.cur.Literal
		p.next()
		fields = append(fields, StructField{Name: fieldName, Type: fieldType})
		if p.cur.Type == lexer.COMMA {
			p.next()
		} else {
			break
		}
	}
	p.expect(lexer.RBRACE)
	return &StructDecl{Name: name, Fields: fields}
}

func (p *Parser) parseGlobal() *GlobalDecl {
	p.expect(lexer.GLOBAL)
	name := p.cur.Literal
	p.next()
	p.expect(lexer.COLON)
	typ := p.cur.Literal
	p.next()
	p.expect(lexer.ASSIGN)
	value := p.ParseExpr()
	return &GlobalDecl{Name: name, Type: typ, Value: value}
}
