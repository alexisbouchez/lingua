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

func (l *Lexer) peek() byte {
	if l.pos >= len(l.input) {
		return 0
	}
	return l.input[l.pos]
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
	case '=':
		if l.peek() == '=' {
			l.readChar()
			tok = Token{Type: EQ, Literal: "=="}
		} else {
			tok = Token{Type: ASSIGN, Literal: "="}
		}
	case '!':
		if l.peek() == '=' {
			l.readChar()
			tok = Token{Type: NEQ, Literal: "!="}
		} else {
			tok = Token{Type: NOT, Literal: "!"}
		}
	case '&':
		if l.peek() == '&' {
			l.readChar()
			tok = Token{Type: AND, Literal: "&&"}
		} else {
			tok = Token{Type: BAND, Literal: "&"}
		}
	case '|':
		if l.peek() == '|' {
			l.readChar()
			tok = Token{Type: OR, Literal: "||"}
		} else {
			tok = Token{Type: BOR, Literal: "|"}
		}
	case '^':
		tok = Token{Type: BXOR, Literal: "^"}
	case '<':
		if l.peek() == '=' {
			l.readChar()
			tok = Token{Type: LTE, Literal: "<="}
		} else if l.peek() == '<' {
			l.readChar()
			tok = Token{Type: SHL, Literal: "<<"}
		} else {
			tok = Token{Type: LT, Literal: "<"}
		}
	case '>':
		if l.peek() == '=' {
			l.readChar()
			tok = Token{Type: GTE, Literal: ">="}
		} else if l.peek() == '>' {
			l.readChar()
			tok = Token{Type: SHR, Literal: ">>"}
		} else {
			tok = Token{Type: GT, Literal: ">"}
		}
	case '"':
		return Token{Type: STRING, Literal: l.readString()}
	default:
		if isLetter(l.ch) {
			lit := l.readIdent()
			return Token{Type: LookupIdent(lit), Literal: lit}
		}
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
