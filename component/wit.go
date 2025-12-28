package component

// WIT (WebAssembly Interface Types) definitions for wasi:http
// Based on https://github.com/WebAssembly/wasi-http

// HTTPMethod represents HTTP methods
type HTTPMethod int

const (
	MethodGet HTTPMethod = iota
	MethodHead
	MethodPost
	MethodPut
	MethodDelete
	MethodConnect
	MethodOptions
	MethodTrace
	MethodPatch
)

// HTTPScheme represents URI schemes
type HTTPScheme int

const (
	SchemeHTTP HTTPScheme = iota
	SchemeHTTPS
	SchemeOther
)

// WasiHTTPTypes defines the wasi:http/types interface
type WasiHTTPTypes struct {
	// Resource types
	Fields          ResourceType
	IncomingRequest ResourceType
	OutgoingRequest ResourceType
	IncomingResponse ResourceType
	OutgoingResponse ResourceType
	IncomingBody    ResourceType
	OutgoingBody    ResourceType
}

// ResourceType represents a WIT resource handle
type ResourceType struct {
	Name string
	ID   int
}

// WasiHTTPOutgoingHandler defines the wasi:http/outgoing-handler interface
// This is used for making HTTP client requests
type WasiHTTPOutgoingHandler struct {
	// handle: func(request: outgoing-request, options: option<request-options>) -> future<result<incoming-response, error-code>>
}

// WasiHTTPIncomingHandler defines the wasi:http/incoming-handler interface
// This is used for handling incoming HTTP server requests
type WasiHTTPIncomingHandler struct {
	// handle: func(request: incoming-request, response-out: response-outparam)
}

// WasiInterface represents a WASI interface import/export
type WasiInterface struct {
	Package   string // e.g., "wasi:http"
	Interface string // e.g., "outgoing-handler"
	Version   string // e.g., "0.2.0"
}

// ParseInterface parses a WASI interface string like "wasi:http/outgoing-handler@0.2.0"
func ParseInterface(s string) WasiInterface {
	// Simple parser - production code would be more robust
	var iface WasiInterface

	// Find package separator
	for i := 0; i < len(s); i++ {
		if s[i] == '/' {
			iface.Package = s[:i]
			s = s[i+1:]
			break
		}
	}

	// Find version separator
	for i := 0; i < len(s); i++ {
		if s[i] == '@' {
			iface.Interface = s[:i]
			iface.Version = s[i+1:]
			return iface
		}
	}

	iface.Interface = s
	return iface
}

// CanonicalABI defines the Canonical ABI for translating between
// component-level types and core WASM types
type CanonicalABI struct{}

// LiftString lifts a string from linear memory
// In the canonical ABI, strings are (ptr, len) pairs in i32
func (abi *CanonicalABI) LiftString() (paramCount int, resultCount int) {
	return 2, 0 // ptr and len as i32
}

// LowerString lowers a string to linear memory
func (abi *CanonicalABI) LowerString() (paramCount int, resultCount int) {
	return 0, 2 // returns ptr and len as i32
}

// LiftList lifts a list from linear memory
func (abi *CanonicalABI) LiftList() (paramCount int, resultCount int) {
	return 2, 0 // ptr and len as i32
}

// LowerList lowers a list to linear memory
func (abi *CanonicalABI) LowerList() (paramCount int, resultCount int) {
	return 0, 2 // returns ptr and len as i32
}

// HTTPRequest represents an HTTP request in Lingua
type HTTPRequest struct {
	Method  HTTPMethod
	URI     string
	Headers []HTTPHeader
	Body    []byte
}

// HTTPResponse represents an HTTP response in Lingua
type HTTPResponse struct {
	Status  int
	Headers []HTTPHeader
	Body    []byte
}

// HTTPHeader represents an HTTP header
type HTTPHeader struct {
	Name  string
	Value string
}

// ErrorCode represents wasi:http error codes
type ErrorCode int

const (
	ErrorDNSTimeout ErrorCode = iota
	ErrorDNSError
	ErrorDestinationNotFound
	ErrorDestinationUnavailable
	ErrorDestinationIPProhibited
	ErrorDestinationIPUnroutable
	ErrorConnectionRefused
	ErrorConnectionTerminated
	ErrorConnectionTimeout
	ErrorConnectionReadTimeout
	ErrorConnectionWriteTimeout
	ErrorConnectionLimitReached
	ErrorTLSProtocolError
	ErrorTLSCertificateError
	ErrorTLSAlertReceived
	ErrorHTTPRequestDenied
	ErrorHTTPRequestBodySize
	ErrorHTTPRequestMethodInvalid
	ErrorHTTPRequestURIInvalid
	ErrorHTTPRequestURITooLong
	ErrorHTTPRequestHeaderSectionSize
	ErrorHTTPRequestHeaderSize
	ErrorHTTPRequestTrailerSectionSize
	ErrorHTTPRequestTrailerSize
	ErrorHTTPResponseIncomplete
	ErrorHTTPResponseHeaderSectionSize
	ErrorHTTPResponseHeaderSize
	ErrorHTTPResponseBodySize
	ErrorHTTPResponseTrailerSectionSize
	ErrorHTTPResponseTrailerSize
	ErrorHTTPResponseTransferCoding
	ErrorHTTPResponseContentCoding
	ErrorHTTPResponseTimeout
	ErrorHTTPUpgradeFailed
	ErrorHTTPProtocolError
	ErrorLoopDetected
	ErrorConfigurationError
	ErrorInternalError
)
