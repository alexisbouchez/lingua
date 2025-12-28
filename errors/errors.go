package errors

import (
	"fmt"
	"strings"
)

type Error struct {
	Message string
	Line    int
	Column  int
	Source  string
}

func New(message string, line, column int) *Error {
	return &Error{
		Message: message,
		Line:    line,
		Column:  column,
	}
}

func (e *Error) Error() string {
	return fmt.Sprintf("error at line %d, column %d: %s", e.Line, e.Column, e.Message)
}

func (e *Error) WithSource(source string) *Error {
	e.Source = source
	return e
}

func (e *Error) Format() string {
	if e.Source == "" {
		return e.Error()
	}

	lines := strings.Split(e.Source, "\n")
	if e.Line < 1 || e.Line > len(lines) {
		return e.Error()
	}

	var b strings.Builder
	b.WriteString(fmt.Sprintf("error at line %d, column %d: %s\n", e.Line, e.Column, e.Message))

	lineNum := e.Line
	sourceLine := lines[lineNum-1]

	b.WriteString(fmt.Sprintf("%4d | %s\n", lineNum, sourceLine))

	pointer := strings.Repeat(" ", 7+e.Column-1) + "^"
	b.WriteString(pointer + "\n")

	return b.String()
}

type ErrorList struct {
	Errors []*Error
	Source string
}

func NewErrorList() *ErrorList {
	return &ErrorList{
		Errors: make([]*Error, 0),
	}
}

func (el *ErrorList) Add(message string, line, column int) {
	err := New(message, line, column)
	err.Source = el.Source
	el.Errors = append(el.Errors, err)
}

func (el *ErrorList) HasErrors() bool {
	return len(el.Errors) > 0
}

func (el *ErrorList) Format() string {
	if !el.HasErrors() {
		return ""
	}

	var b strings.Builder
	for i, err := range el.Errors {
		if i > 0 {
			b.WriteString("\n")
		}
		b.WriteString(err.Format())
	}
	return b.String()
}
