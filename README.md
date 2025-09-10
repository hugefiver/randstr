# randstr

A Racket library and command-line tool for generating random strings based on regex-like patterns.

## Installation

To install, run:
```
raco pkg install randstr
```

## Usage

### As a Library

```racket
(require randstr)

(randstr "[a-z]{5}")  ; Generate a random 5-letter lowercase string
(randstr "(abc|def)+")  ; Generate a string with repeated "abc" or "def"
(randstr* "[0-9]{3}" 10)  ; Generate 10 random 3-digit numbers
```

### As a Command-Line Tool

```bash
randstr "[a-z]{5}"          # Generate one random string
randstr -n 10 "[0-9]{3}"    # Generate 10 random 3-digit numbers
```

## Pattern Syntax

- `[abc]` - Choose randomly from characters a, b, or c
- `[a-z]` - Choose randomly from lowercase letters a through z
- `(abc|def)` - Choose randomly between "abc" or "def"
- `a*` - Zero or more of the preceding character
- `a+` - One or more of the preceding character
- `a?` - Zero or one of the preceding character
- `.` - Any character

## Examples

```racket
(randstr "[a-z]{5}")     ; => "kxmpr"
(randstr "[0-9][a-z]+")  ; => "3xkzm"
(randstr "(abc|def)+")   ; => "abcdefabc"
```