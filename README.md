# kaappi-csv

CSV parser and writer for [Kaappi Scheme](https://github.com/kaappi/kaappi).

Pure Scheme — no C dependencies, no build step. RFC 4180 compliant.

## Install

```bash
thottam install kaappi-csv
```

## Quick start

```scheme
(import (kaappi csv))

;; Parse with headers — rows become alists
(define data (csv-read-string
  "name,age\nAlice,30\nBob,25"
  'headers #t))

(cdr (assoc "name" (car data)))  ;=> "Alice"

;; Parse without headers — rows are lists of strings
(csv-read-string "a,b,c\n1,2,3")
;=> (("a" "b" "c") ("1" "2" "3"))

;; Write
(csv-write-string '(("Name" "Score") ("Alice" "95")))
;=> "Name,Score\r\nAlice,95\r\n"
```

## API

### Reading

```scheme
(csv-read port [options...])          ; parse all rows from port
(csv-read-string string [options...]) ; parse all rows from string
(csv-read-row port [delimiter])       ; parse single row, returns #f at EOF
```

**Options** (keyword-style):

- `'delimiter char` — field separator (default: `#\,`)
- `'headers #t` — treat first row as headers; return rows as alists

### Writing

```scheme
(csv-write rows port [options...])          ; write rows to port
(csv-write-string rows [options...])        ; write rows to string
(csv-write-row row port [delimiter])        ; write single row
```

Fields containing commas, quotes, or newlines are automatically quoted.
Numbers and booleans are coerced to strings.

### Streaming

```scheme
(csv-fold port proc init [options...])
```

Process rows one at a time without building the full list in memory.
Supports `'headers #t` for alist rows.

## Features

- RFC 4180 compliant (quoted fields, escaped quotes, CRLF)
- Custom delimiters (tab-separated, semicolon, etc.)
- Header mode — rows as alists for named access
- Streaming fold for large files
- Round-trip safe (read then write preserves data)
- Handles embedded newlines in quoted fields

## License

MIT
