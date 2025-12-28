package lexer

type Lexer struct {
	input string
	pos   int
	ch    byte
}

func New(input string) *Lexer {
	l := &Lexer{input: input}
	l.readChar()
	return l
}

func (l *Lexer) readChar() {
	if l.pos >= len(l.input) {
		l.ch = 0
	} else {
		l.ch = l.input[l.pos]
	}
	l.pos++
}

func (l *Lexer) NextToken() Token {
	l.skipWhitespace()

	if l.ch == 0 {
		return Token{Type: EOF}
	}

	var tok Token
	switch l.ch {
	case '+':
		tok = Token{Type: PLUS, Literal: "+"}
	case '-':
		tok = Token{Type: MINUS, Literal: "-"}
	case '*':
		tok = Token{Type: STAR, Literal: "*"}
	case '/':
		tok = Token{Type: SLASH, Literal: "/"}
	case '%':
		tok = Token{Type: PERCENT, Literal: "%"}
	case '(':
		tok = Token{Type: LPAREN, Literal: "("}
	case ')':
		tok = Token{Type: RPAREN, Literal: ")"}
	case '{':
		tok = Token{Type: LBRACE, Literal: "{"}
	case '}':
		tok = Token{Type: RBRACE, Literal: "}"}
	case ',':
		tok = Token{Type: COMMA, Literal: ","}
	case ';':
		tok = Token{Type: SEMI, Literal: ";"}
	case ':':
		tok = Token{Type: COLON, Literal: ":"}
	default:
		if isDigit(l.ch) {
			return Token{Type: INT, Literal: l.readNumber()}
		}
		return Token{Type: EOF}
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

func isDigit(ch byte) bool {
	return ch >= '0' && ch <= '9'
}
