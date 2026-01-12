# Changelog

## Unreleased

### Added
- ✨ New: Cryptographically secure random number generator option
  - Library parameter `randstr-secure-random?` to enable secure random mode
  - CLI option `-s/--secure` to use cryptographically secure random
  - Environment variable `RANDSTR_SECURE` to enable secure random from environment

## 0.2.0 (2026-01-09)

### Added
- CLI option `-m/--max-repeat` and env var `RANDSTR_MAX_REPEAT` to control the maximum repetition cap used by `*` and `+`.
- Library parameter `randstr-max-repeat` for configuring the `*`/`+` repetition cap programmatically.
- Character class enhancements:
  - Negated classes like `[^abc]` (complement within ASCII printable characters).
  - Common escapes inside `[...]`: `\d \D \w \W \s \S`, plus literal escapes like `\] \- \\`.

### Changed
- `tokenize-pattern` was refactored to avoid repeated `length` calls, improving performance on longer patterns.
- `parse-quantifier` contracts were tightened to reflect its actual return shapes (`n`, `(normal ...)`, `(normal-range ...)`).

### Fixed
- Group alternation parsing now only splits on top-level `|` (doesn't break nested groups or `|` inside character classes).
- Unicode filtering iteration off-by-one in property generation.
