#lang scribble/manual

@title{randstr: Random String Generator}
@author{Hugefiver}

@defmodule[randstr]

A library for generating random strings based on regex-like patterns.

@section{Functions}

@defproc[(randstr [pattern string?]) string?]{
  Generate a random string based on the given pattern.
  
  Examples:
  @racketblock[
  (randstr "[a-z]{5}")  ; => "kxmpr"
  (randstr "[0-9][a-z]+")  ; => "3xkzm"
  (randstr "(abc|def)+")   ; => "abcdefabc"
  ]
}

@defproc[(randstr* [pattern string?] [n exact-positive-integer?]) (listof string?)]{
  Generate a list of @racket[n] random strings based on the given pattern.
  
  Examples:
  @racketblock[
  (randstr* "[0-9]{3}" 5)  ; => ("123" "456" "789" "012" "345")
  ]
}

@section{Pattern Syntax}

The following pattern syntax is supported:

@itemlist[
  @item{@litchar{[abc]} - Choose randomly from characters a, b, or c}
  @item{@litchar{[a-z]} - Choose randomly from lowercase letters a through z}
  @item{@litchar{(abc|def)} - Choose randomly between "abc" or "def"}
  @item{@litchar{a*} - Zero or more of the preceding character}
  @item{@litchar{a+} - One or more of the preceding character}
  @item{@litchar{a?} - Zero or one of the preceding character}
  @item{@litchar{.} - Any character}
  @item{@litchar{[:alpha:]} - Alphabetic characters}
  @item{@litchar{[:digit:]} - Numeric characters}
  @item{@litchar{[:alnum:]} - Alphanumeric characters}
  @item{@litchar{[:word:]} - Word characters (alphanumeric plus underscore)}
  @item{@litchar{[:blank:]} - Blank characters (space and tab)}
  @item{@litchar{[:space:]} - Whitespace characters}
  @item{@litchar{[:upper:]} - Uppercase letters}
  @item{@litchar{[:lower:]} - Lowercase letters}
  @item{@litchar{[:ascii:]} - ASCII characters}
  @item{@litchar{[:cntrl:]} - Control characters}
  @item{@litchar{[:graph:]} - Printable characters except space}
  @item{@litchar{[:print:]} - Printable characters including space}
  @item{@litchar{[:punct:]} - Punctuation characters}
  @item{@litchar{[:xdigit:]} - Hexadecimal digits}
]

@section{Advanced Examples}

In addition to basic pattern matching, the library supports more complex patterns:

(randstr "[[:alpha:]]{5}")     ; => "abcde" (5 alphabetic characters)
(randstr "[[:digit:]]{3}")     ; => "123" (3 digits)
(randstr "[[:alnum:]]{4}")     ; => "a1B2" (4 alphanumeric characters)
(randstr "[[:word:]]+")        ; => "hello123_" (word characters)
(randstr "[[:upper:]0-9]+")    ; => "A3B9C" (uppercase letters and digits)
(randstr "[[:lower:]_]+")      ; => "hello_world" (lowercase letters and underscores)
(randstr "[[:alpha:]0-9]+")    ; => "abc123XYZ" (alphabetic characters and digits)

@section{Character Class Duplicate Handling}

When a character class contains duplicate elements, each unique character is treated equally regardless of how many times it appears in the class. For example:

@itemlist[
  @item{@litchar{[aaabbbccc]} - Each of a, b, c has equal probability (1/3 each), not a=3/9, b=3/9, c=3/9}
  @item{@litchar{[a-cb-e]} - Each of a, b, c, d, e has equal probability (1/5 each)}
  @item{@litchar{[[:digit:]0-2]} - Digits 0, 1, 2 appear in both the POSIX class and the range, but each digit still has equal probability}
]

This ensures fair distribution of character selection in all character classes.
]

@section{License}

This project is licensed under the MIT License. See the @filepath{LICENSE} file for details.