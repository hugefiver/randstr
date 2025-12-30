# Copilot Instructions for randstr

## Project Overview
**randstr** is a Racket library and CLI tool for generating random strings from regex-like patterns. It parses patterns, tokenizes them into structured tokens, and generates random strings that match those patterns.

## Architecture

### Core Pipeline: Pattern → Tokens → String
```
Pattern (string) → tokenizer.rkt → token structs → generator.rkt → random string
```

**Key modules:**
- [randstr/main.rkt](randstr/main.rkt) - Public API (`randstr`, `randstr*`), re-exports internal modules
- [randstr/tokenizer.rkt](randstr/tokenizer.rkt) - Pattern parsing, defines `token` struct with `(type content quantifier)`
- [randstr/generator.rkt](randstr/generator.rkt) - Generates strings from token lists via `generate-from-tokens`
- [randstr/char-classes.rkt](randstr/char-classes.rkt) - Character class definitions (POSIX, Unicode properties, escape sequences)
- [randstr/cli/main.rkt](randstr/cli/main.rkt) - Command-line interface using `racket/cmdline`

### Token Structure
Tokens use the struct `(token type content quantifier)`:
- `type`: `'literal`, `'char-class`, `'word-char`, `'digit-char`, `'group`, `'any`, etc.
- `content`: The character, vector of chars, or group content
- `quantifier`: `#f`, `'star`, `'plus`, `'optional`, or an integer for `{n}`

## Developer Commands

```shell
# Run all tests
just test

# Run specific test suites
just test-main           # Main tests (randstr/tests/test.rkt)
just test-extensions     # Extension tests

# Compile the project
just compile

# Build standalone executable
just build               # Creates randstr.exe

# Install as Racket package
just install
```

## Code Conventions

### Module Prefixes
Internal imports use prefixes to avoid conflicts:
```racket
(require (prefix-in tokenizer: "tokenizer.rkt")
         (prefix-in cc: "char-classes.rkt")
         (prefix-in gen: "generator.rkt"))
```

### Contracts
All public APIs use `contract-out` for type safety:
```racket
(provide
 (contract-out
  [randstr (string? . -> . string?)]
  [randstr* (string? exact-positive-integer? . -> . (listof string?))]))
```

### Character Classes Use Vectors
Character class options are stored as vectors (not lists) for performance:
```racket
(token 'char-class #(#\a #\b #\c) #f)
```
Use `vector-random-ref` from char-classes.rkt for random selection.

### Duplicate Handling
Character classes automatically deduplicate - `[aaabbb]` treats each unique char equally.

## Adding New Features

### New Escape Sequence (e.g., `\x`)
1. Add case in `tokenize-pattern` in [randstr/tokenizer.rkt](randstr/tokenizer.rkt#L27-L43)
2. Add character generator in [randstr/char-classes.rkt](randstr/char-classes.rkt)
3. Handle token type in `generate-from-tokens` in [randstr/generator.rkt](randstr/generator.rkt#L57)
4. Add tests in [randstr/tests/test.rkt](randstr/tests/test.rkt)

### New POSIX Class (e.g., `[:newclass:]`)
1. Add class definition function in [randstr/char-classes.rkt](randstr/char-classes.rkt)
2. Add case in `parse-character-class` POSIX handling section in tokenizer.rkt
3. Add tests for the new class

## Testing Patterns
Tests use `rackunit` and validate that generated strings match expected regex:
```racket
(check-true (string? (randstr "[a-z]{5}")))
```

For property-based validation, see [test-helpers.rkt](randstr/tests/test-helpers.rkt) which provides `make-pattern-checker`.
