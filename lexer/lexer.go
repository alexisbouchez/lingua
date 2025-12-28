package lexer

type Lexer struct {
	input  string
	pos    int
	ch     byte
	line   int
	column int
}

func New(input string) *Lexer {
	l := &Lexer{input: input, line: 1, column: 0}
	l.readChar()
	return l
}

func (l *Lexer) readChar() {
	if l.ch == '\n' {
		l.line++
		l.column = 0
	}
	if l.pos >= len(l.input) {
		l.ch = 0
	} else {
		l.ch = l.input[l.pos]
	}
	l.pos++
	l.column++
}

func (l *Lexer) peek() byte {
	if l.pos >= len(l.input) {
		return 0
	}
	return l.input[l.pos]
}

func (l *Lexer) NextToken() Token {
	l.skipWhitespace()

	line := l.line
	col := l.column

	if l.ch == 0 {
		return Token{Type: EOF, Line: line, Column: col}
	}

	var tok Token
	switch l.ch {
	case '+':
		tok = Token{Type: PLUS, Literal: "+", Line: line, Column: col}
	case '-':
		tok = Token{Type: MINUS, Literal: "-", Line: line, Column: col}
	case '*':
		tok = Token{Type: STAR, Literal: "*", Line: line, Column: col}
	case '/':
		tok = Token{Type: SLASH, Literal: "/", Line: line, Column: col}
	case '%':
		tok = Token{Type: PERCENT, Literal: "%", Line: line, Column: col}
	case '(':
		tok = Token{Type: LPAREN, Literal: "(", Line: line, Column: col}
	case ')':
		tok = Token{Type: RPAREN, Literal: ")", Line: line, Column: col}
	case '{':
		tok = Token{Type: LBRACE, Literal: "{", Line: line, Column: col}
	case '}':
		tok = Token{Type: RBRACE, Literal: "}", Line: line, Column: col}
	case '[':
		tok = Token{Type: LBRACKET, Literal: "[", Line: line, Column: col}
	case ']':
		tok = Token{Type: RBRACKET, Literal: "]", Line: line, Column: col}
	case ',':
		tok = Token{Type: COMMA, Literal: ",", Line: line, Column: col}
	case ';':
		tok = Token{Type: SEMI, Literal: ";", Line: line, Column: col}
	case ':':
		tok = Token{Type: COLON, Literal: ":", Line: line, Column: col}
	case '=':
		if l.peek() == '=' {
			l.readChar()
			tok = Token{Type: EQ, Literal: "==", Line: line, Column: col}
		} else {
			tok = Token{Type: ASSIGN, Literal: "=", Line: line, Column: col}
		}
	case '!':
		if l.peek() == '=' {
			l.readChar()
			tok = Token{Type: NEQ, Literal: "!=", Line: line, Column: col}
		} else {
			tok = Token{Type: NOT, Literal: "!", Line: line, Column: col}
		}
	case '&':
		if l.peek() == '&' {
			l.readChar()
			tok = Token{Type: AND, Literal: "&&", Line: line, Column: col}
		} else {
			tok = Token{Type: BAND, Literal: "&", Line: line, Column: col}
		}
	case '|':
		if l.peek() == '|' {
			l.readChar()
			tok = Token{Type: OR, Literal: "||", Line: line, Column: col}
		} else {
			tok = Token{Type: BOR, Literal: "|", Line: line, Column: col}
		}
	case '^':
		tok = Token{Type: BXOR, Literal: "^", Line: line, Column: col}
	case '<':
		if l.peek() == '=' {
			l.readChar()
			tok = Token{Type: LTE, Literal: "<=", Line: line, Column: col}
		} else if l.peek() == '<' {
			l.readChar()
			tok = Token{Type: SHL, Literal: "<<", Line: line, Column: col}
		} else {
			tok = Token{Type: LT, Literal: "<", Line: line, Column: col}
		}
	case '>':
		if l.peek() == '=' {
			l.readChar()
			tok = Token{Type: GTE, Literal: ">=", Line: line, Column: col}
		} else if l.peek() == '>' {
			l.readChar()
			tok = Token{Type: SHR, Literal: ">>", Line: line, Column: col}
		} else {
			tok = Token{Type: GT, Literal: ">", Line: line, Column: col}
		}
	case '"':
		lit := l.readString()
		return Token{Type: STRING, Literal: lit, Line: line, Column: col}
	default:
		if isLetter(l.ch) {
			lit := l.readIdent()
			return Token{Type: LookupIdent(lit), Literal: lit, Line: line, Column: col}
		}
		if isDigit(l.ch) {
			lit := l.readNumber()
			return Token{Type: INT, Literal: lit, Line: line, Column: col}
		}
		return Token{Type: EOF, Line: line, Column: col}
	}
	l.readChar()
	return tok
}

func (l *Lexer) skipWhitespace() {
	for l.ch == ' ' || l.ch == '\t' || l.ch == '\n' || l.ch == '\r' {
		l.readChar()
	}
}

func (l *Lexer) readNumber() string {
	start := l.pos - 1
	for isDigit(l.ch) {
		l.readChar()
	}
	return l.input[start : l.pos-1]
}

func (l *Lexer) readIdent() string {
	start := l.pos - 1
	for isLetter(l.ch) || isDigit(l.ch) {
		l.readChar()
	}
	return l.input[start : l.pos-1]
}

func (l *Lexer) readString() string {
	l.readChar() // skip opening quote
	var result []byte
	for l.ch != '"' && l.ch != 0 {
		if l.ch == '\\' {
			l.readChar()
			switch l.ch {
			case 'n':
				result = append(result, '\n')
			case 't':
				result = append(result, '\t')
			case 'r':
				result = append(result, '\r')
			case '\\':
				result = append(result, '\\')
			case '"':
				result = append(result, '"')
			default:
				result = append(result, l.ch)
			}
		} else {
			result = append(result, l.ch)
		}
		l.readChar()
	}
	l.readChar() // skip closing quote
	return string(result)
}

func isDigit(ch byte) bool {
	return ch >= '0' && ch <= '9'
}

func isLetter(ch byte) bool {
	return (ch >= 'a' && ch <= 'z') || (ch >= 'A' && ch <= 'Z') || ch == '_'
}
