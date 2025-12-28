// Package source provides source mapping and debugging information for Lingua
package source

import (
	"encoding/json"
	"fmt"
)

// Position represents a source code position (line, column)
type Position struct {
	Line   int `json:"line"`
	Column int `json:"column"`
}

// SourceMap represents a mapping from compiled code to source code
// This is a simplified source map format for Lingua
// In a real implementation, this would follow the Source Map v3 specification
type SourceMap struct {
	Version    int        `json:"version"`
	File       string     `json:"file"`
	SourceRoot string     `json:"sourceRoot,omitempty"`
	Sources    []string   `json:"sources"`
	Mappings   string     `json:"mappings"`
	Names      []string   `json:"names,omitempty"`
}

// Mapping represents a single source mapping entry
// This is used internally before generating the VLQ-encoded mappings string
type Mapping struct {
	GeneratedLine   int
	GeneratedColumn int
	OriginalLine     int
	OriginalColumn   int
	SourceIndex      int
	NameIndex        int
}

// SourceMapBuilder builds source maps incrementally
// This is used during compilation to track source positions
type SourceMapBuilder struct {
	fileName    string
	sources     []string
	mappings    []Mapping
	names       []string
	nameToIndex map[string]int
}

// NewSourceMapBuilder creates a new source map builder
func NewSourceMapBuilder(fileName string) *SourceMapBuilder {
	return &SourceMapBuilder{
		fileName:    fileName,
		sources:     []string{fileName},
		mappings:    make([]Mapping, 0),
		names:       make([]string, 0),
		nameToIndex: make(map[string]int),
	}
}

// AddMapping adds a source mapping entry
func (b *SourceMapBuilder) AddMapping(
	generatedLine, generatedColumn int,
	originalLine, originalColumn int,
	sourceIndex int,
	name string,
) {
	nameIndex := -1
	if name != "" {
		if idx, ok := b.nameToIndex[name]; ok {
			nameIndex = idx
		} else {
			nameIndex = len(b.names)
			b.names = append(b.names, name)
			b.nameToIndex[name] = nameIndex
		}
	}

	b.mappings = append(b.mappings, Mapping{
		GeneratedLine:   generatedLine,
		GeneratedColumn: generatedColumn,
		OriginalLine:     originalLine,
		OriginalColumn:   originalColumn,
		SourceIndex:      sourceIndex,
		NameIndex:        nameIndex,
	})
}

// Build generates the final source map
func (b *SourceMapBuilder) Build() *SourceMap {
	// For now, we'll generate a simple source map
	// In a real implementation, we would encode the mappings
	// using VLQ (Variable Length Quantity) encoding
	
	return &SourceMap{
		Version:    3,
		File:       b.fileName,
		Sources:    b.sources,
		Mappings:   "", // TODO: Implement VLQ encoding
		Names:      b.names,
	}
}

// JSON returns the source map as JSON
func (sm *SourceMap) JSON() (string, error) {
	bytes, err := json.MarshalIndent(sm, "", "  ")
	if err != nil {
		return "", err
	}
	return string(bytes), nil
}

// String returns a string representation of the source map
func (sm *SourceMap) String() string {
	jsonStr, _ := sm.JSON()
	return jsonStr
}

// PositionToString converts a position to a string
func (p Position) String() string {
	return fmt.Sprintf("%d:%d", p.Line, p.Column)
}