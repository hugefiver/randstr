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
- `\w` - Word character (alphanumeric plus underscore)
- `\W` - Non-word character
- `\s` - Whitespace character (space, tab, newline, carriage return)
- `\S` - Non-whitespace character
- `\d` - Digit character (0-9)
- `\D` - Non-digit character
- `[:alpha:]` - Alphabetic characters
- `[:digit:]` - Numeric characters
- `[:alphanum:]` - Alphanumeric characters
- `[:alnum:]` - Alphanumeric characters (POSIX standard name)
- `[:word:]` - Word characters (alphanumeric plus underscore)
- `[:blank:]` - Blank characters (space and tab)
- `[:space:]` - Whitespace characters
- `[:upper:]` - Uppercase letters
- `[:lower:]` - Lowercase letters
- `[:ascii:]` - ASCII characters
- `[:cntrl:]` - Control characters
- `[:graph:]` - Printable characters except space
- `[:print:]` - Printable characters including space
- `[:punct:]` - Punctuation characters
- `[:xdigit:]` - Hexadecimal digits

### Nested POSIX Character Classes

The library also supports nested POSIX character classes, allowing you to mix POSIX classes with regular characters:

- `[[:upper:]0-9]` - Uppercase letters and digits
- `[[:lower:]_]` - Lowercase letters and underscores
- `[[:alpha:]0-9]` - Alphabetic characters and digits

### Character Class Duplicate Handling

When a character class contains duplicate elements, each unique character is treated equally regardless of how many times it appears in the class. For example:

- `[aaabbbccc]` - Each of a, b, c has equal probability (1/3 each), not a=3/9, b=3/9, c=3/9
- `[a-cb-e]` - Each of a, b, c, d, e has equal probability (1/5 each)
- `[[:digit:]0-2]` - Digits 0, 1, 2 appear in both the POSIX class and the range, but each digit still has equal probability

This ensures fair distribution of character selection in all character classes.

## Examples

```racket
(randstr "[a-z]{5}")     ; => "kxmpr"
(randstr "[0-9][a-z]+")  ; => "3xkzm"
(randstr "(abc|def)+")   ; => "abcdefabc"
(randstr "\\w+")         ; => "abc123_"
(randstr "\\W+")         ; => "!@#$%"
(randstr "\\d+")         ; => "12345"
(randstr "\\D+")         ; => "abcde"
(randstr "[[:alpha:]]+") ; => "abcXYZ"
(randstr "[[:digit:]]+") ; => "12345"
(randstr "[[:alnum:]]+") ; => "abc123"
(randstr "[[:word:]]+")  ; => "abc123_"
(randstr "[[:blank:]]*") ; => "   "
(randstr "[[:upper:]]+") ; => "ABCXYZ"
(randstr "[[:lower:]]+") ; => "abcxyz"
(randstr "[[:xdigit:]]+") ; => "1A2B3C"
(randstr "[[:upper:]0-9]+") ; => "A3B9C"
(randstr "[[:lower:]_]+") ; => "abc_def"
(randstr "[[:alpha:]0-9]+") ; => "abc123XYZ"
```