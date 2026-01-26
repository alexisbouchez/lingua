# WASI Core Interfaces Reference

## I/O: Poll Interface (`wasi:io/poll`)

Enables waiting for I/O events on multiple handles:

```wit
package wasi:io@0.2.8;

interface poll {
    resource pollable {
        // Check readiness without blocking
        ready: func() -> bool;

        // Block until ready
        block: func();
    }

    // Wait for any pollable to be ready
    // Returns indices of ready pollables
    poll: func(in: list<borrow<pollable>>) -> list<u32>;
}
```

**Usage Notes:**
- `poll` traps if the list is empty
- Timeout via wasi-clocks pollable
- Ready state indicates I/O is possible, not error-free

## Clocks: Wall Clock (`wasi:clocks/wall-clock`)

Real-world time (non-monotonic):

```wit
package wasi:clocks@0.2.8;

interface wall-clock {
    record datetime {
        seconds: u64,      // Since Unix epoch
        nanoseconds: u32,  // Always < 1,000,000,000
    }

    // Current time (may go backwards!)
    now: func() -> datetime;

    // Clock precision
    resolution: func() -> datetime;
}
```

**Design Notes:**
- Uses Unix epoch (1970-01-01T00:00:00Z)
- Not monotonic - external references may reset
- For elapsed time measurement, use monotonic-clock instead

## Clocks: Monotonic Clock (`wasi:clocks/monotonic-clock`)

```wit
interface monotonic-clock {
    type instant = u64;  // Nanoseconds
    type duration = u64; // Nanoseconds

    now: func() -> instant;
    resolution: func() -> duration;

    // Create pollable that resolves at given instant
    subscribe-instant: func(when: instant) -> pollable;

    // Create pollable that resolves after duration
    subscribe-duration: func(when: duration) -> pollable;
}
```

## Random (`wasi:random/random`)

Cryptographically secure random data:

```wit
package wasi:random@0.2.8;

interface random {
    // Get cryptographically secure random bytes
    // Never blocks, always returns fresh data
    get-random-bytes: func(len: u64) -> list<u8>;

    // Get random u64
    get-random-u64: func() -> u64;
}
```

**Requirements:**
- Must be CSPRNG quality
- Must never block
- Must always return unpredictable data
- Deterministic environments must omit this interface

## Filesystem Types (`wasi:filesystem/types`)

Core filesystem abstractions:

```wit
package wasi:filesystem@0.2.8;

interface types {
    type filesize = u64;
    type link-count = u64;

    enum descriptor-type {
        unknown,
        block-device,
        character-device,
        directory,
        fifo,
        symbolic-link,
        regular-file,
        socket,
    }

    flags descriptor-flags {
        read,
        write,
        file-integrity-sync,
        data-integrity-sync,
        requested-write-sync,
        mutate-directory,
    }

    resource descriptor {
        // Stream operations
        read-via-stream: func() -> result<input-stream, error-code>;
        write-via-stream: func() -> result<output-stream, error-code>;

        // Metadata
        stat: func() -> result<descriptor-stat, error-code>;
        get-type: func() -> result<descriptor-type, error-code>;

        // File operations
        read: func(len: filesize, offset: filesize)
            -> result<tuple<list<u8>, bool>, error-code>;
        write: func(buf: list<u8>, offset: filesize)
            -> result<filesize, error-code>;

        // Directory operations
        read-directory: func() -> result<directory-entry-stream, error-code>;
        create-directory-at: func(path: string) -> result<_, error-code>;
        open-at: func(
            path-flags: path-flags,
            path: string,
            open-flags: open-flags,
            flags: descriptor-flags,
        ) -> result<descriptor, error-code>;
    }

    enum error-code {
        access,
        would-block,
        already,
        bad-descriptor,
        busy,
        deadlock,
        quota,
        exist,
        file-too-large,
        illegal-byte-sequence,
        in-progress,
        interrupted,
        invalid,
        io,
        is-directory,
        loop,
        too-many-links,
        message-size,
        name-too-long,
        no-device,
        no-entry,
        no-lock,
        insufficient-memory,
        insufficient-space,
        not-directory,
        not-empty,
        not-recoverable,
        unsupported,
        no-tty,
        no-such-device,
        overflow,
        not-permitted,
        pipe,
        read-only,
        invalid-seek,
        text-file-busy,
        cross-device,
    }
}
```

## Sockets: TCP (`wasi:sockets/tcp`)

TCP networking interface:

```wit
package wasi:sockets@0.2.8;

interface tcp {
    resource tcp-socket {
        // Connection lifecycle
        start-bind: func(
            network: borrow<network>,
            local-address: ip-socket-address,
        ) -> result<_, error-code>;
        finish-bind: func() -> result<_, error-code>;

        start-connect: func(
            network: borrow<network>,
            remote-address: ip-socket-address,
        ) -> result<_, error-code>;
        finish-connect: func()
            -> result<tuple<input-stream, output-stream>, error-code>;

        start-listen: func() -> result<_, error-code>;
        finish-listen: func() -> result<_, error-code>;

        accept: func()
            -> result<tuple<tcp-socket, input-stream, output-stream>, error-code>;

        // Socket properties
        local-address: func() -> result<ip-socket-address, error-code>;
        remote-address: func() -> result<ip-socket-address, error-code>;

        set-keep-alive-enabled: func(value: bool) -> result<_, error-code>;
        keep-alive-enabled: func() -> result<bool, error-code>;

        // Async support
        subscribe: func() -> pollable;

        // Graceful shutdown
        shutdown: func(shutdown-type: shutdown-type) -> result<_, error-code>;
    }

    enum shutdown-type {
        receive,
        send,
        both,
    }
}
```

**Design Notes:**
- Async bind/connect enables permission prompts
- Socket states: unbound → bound → listening/connecting → connected
- All operations are non-blocking with pollable integration

## HTTP Types (`wasi:http/types`)

HTTP request/response abstractions:

```wit
package wasi:http@0.2.8;

interface types {
    variant method {
        get, head, post, put, delete,
        connect, options, trace, patch,
        other(string),
    }

    variant scheme {
        HTTP, HTTPS,
        other(string),
    }

    resource fields {
        constructor();
        get: func(name: field-key) -> list<field-value>;
        set: func(name: field-key, value: list<field-value>) -> result;
        append: func(name: field-key, value: field-value) -> result;
        delete: func(name: field-key);
        entries: func() -> list<tuple<field-key, field-value>>;
    }

    resource outgoing-request {
        constructor(headers: fields);
        method: func() -> method;
        set-method: func(method: method) -> result;
        path-with-query: func() -> option<string>;
        set-path-with-query: func(path: option<string>) -> result;
        scheme: func() -> option<scheme>;
        set-scheme: func(scheme: option<scheme>) -> result;
        authority: func() -> option<string>;
        set-authority: func(authority: option<string>) -> result;
        body: func() -> result<outgoing-body, _>;
    }

    resource incoming-response {
        status: func() -> status-code;
        headers: func() -> headers;
        consume: func() -> result<incoming-body, _>;
    }

    resource request-options {
        constructor();
        connect-timeout: func() -> option<duration>;
        set-connect-timeout: func(duration: option<duration>) -> result;
        first-byte-timeout: func() -> option<duration>;
        set-first-byte-timeout: func(duration: option<duration>) -> result;
        between-bytes-timeout: func() -> option<duration>;
        set-between-bytes-timeout: func(duration: option<duration>) -> result;
    }

    variant error-code {
        DNS-timeout,
        DNS-error(DNS-error-payload),
        destination-not-found,
        destination-unavailable,
        destination-IP-prohibited,
        destination-IP-unroutable,
        connection-refused,
        connection-terminated,
        connection-timeout,
        connection-read-timeout,
        connection-write-timeout,
        connection-limit-reached,
        TLS-protocol-error,
        TLS-certificate-error,
        TLS-alert-received(TLS-alert-received-payload),
        HTTP-request-denied,
        HTTP-request-length-required,
        HTTP-request-body-size(option<u64>),
        HTTP-request-method-invalid,
        HTTP-request-URI-invalid,
        HTTP-request-URI-too-long,
        HTTP-request-header-section-size(option<u32>),
        HTTP-request-header-size(option<field-size-payload>),
        HTTP-request-trailer-section-size(option<u32>),
        HTTP-request-trailer-size(field-size-payload),
        HTTP-response-incomplete,
        HTTP-response-header-section-size(option<u32>),
        HTTP-response-header-size(field-size-payload),
        HTTP-response-body-size(option<u64>),
        HTTP-response-trailer-section-size(option<u32>),
        HTTP-response-trailer-size(field-size-payload),
        HTTP-response-transfer-coding(option<string>),
        HTTP-response-content-coding(option<string>),
        HTTP-response-timeout,
        HTTP-upgrade-failed,
        HTTP-protocol-error,
        loop-detected,
        configuration-error,
        internal-error(option<string>),
    }
}
```

## CLI World (`wasi:cli/command`)

Command-line application world:

```wit
package wasi:cli@0.2.8;

world imports {
    include wasi:clocks/imports@0.2.8;
    include wasi:filesystem/imports@0.2.8;
    include wasi:sockets/imports@0.2.8;
    include wasi:random/imports@0.2.8;
    include wasi:io/imports@0.2.8;

    import environment;
    import exit;
    import stdin;
    import stdout;
    import stderr;
    import terminal-input;
    import terminal-output;
    import terminal-stdin;
    import terminal-stdout;
    import terminal-stderr;
}

world command {
    include imports;
    export run;
}
```
