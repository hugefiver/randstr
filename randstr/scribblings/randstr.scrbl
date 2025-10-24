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
  @item{@litchar{\\p{L}} - Unicode letters}
  @item{@litchar{\\p{N}} - Unicode numbers}
  @item{@litchar{\\p{P}} - Unicode punctuation}
  @item{@litchar{\\p{M}} - Unicode marks}
  @item{@litchar{\\p{S}} - Unicode symbols}
  @item{@litchar{\\p{Z}} - Unicode separators}
  @item{@litchar{\\p{C}} - Unicode other (control characters)}
  @item{@litchar{\\p{Lu}} - Unicode uppercase letters}
  @item{@litchar{\\p{Ll}} - Unicode lowercase letters}
  @item{@litchar{\\p{Nd}} - Unicode decimal numbers}
  @item{@litchar{\\p{Letter}} - Unicode letters (alias for \\p{L})}
  @item{@litchar{\\p{Number}} - Unicode numbers (alias for \\p{N})}
  @item{@litchar{\\p{Punctuation}} - Unicode punctuation (alias for \\p{P})}
  @item{@litchar{\\p{Script=Han}} - Unicode characters from Han script}
  @item{@litchar{\\p{Script=Latin}} - Unicode characters from Latin script}
  @item{@litchar{\\p{Block=Basic_Latin}} - Unicode characters from Basic Latin block}
  @item{@litchar{\\p{Block=CJK_Unified_Ideographs}} - Unicode characters from CJK Unified Ideographs block}
  @item{@litchar{\\p{Alphabetic}} - Unicode alphabetic characters}
  @item{@litchar{\\p{Uppercase}} - Unicode uppercase characters}
  @item{@litchar{\\p{Lowercase}} - Unicode lowercase characters}
  @item{@litchar{\\p{White_Space}} - Unicode whitespace characters}
  @item{@litchar{\\p{Cased}} - Unicode characters with case distinctions}
  @item{@litchar{\\p{Dash}} - Unicode dash characters}
  @item{@litchar{\\p{Emoji}} - Unicode emoji characters}
  @item{@litchar{\\p{Emoji_Component}} - Unicode emoji component characters}
  @item{@litchar{\\p{Emoji_Modifier}} - Unicode emoji modifier characters}
  @item{@litchar{\\p{Emoji_Modifier_Base}} - Unicode emoji modifier base characters}
  @item{@litchar{\\p{Emoji_Presentation}} - Unicode emoji presentation characters}
  @item{@litchar{\\p{Extended_Pictographic}} - Unicode extended pictographic characters}
  @item{@litchar{\\p{Hex_Digit}} - Unicode hexadecimal digits}
  @item{@litchar{\\p{ID_Continue}} - Unicode identifier continuation characters}
  @item{@litchar{\\p{ID_Start}} - Unicode identifier start characters}
  @item{@litchar{\\p{Ideographic}} - Unicode ideographic characters}
  @item{@litchar{\\p{Math}} - Unicode mathematical symbols}
  @item{@litchar{\\p{Quotation_Mark}} - Unicode quotation mark characters}
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
(randstr "\\p{L}{5}")          ; => "abcde" (5 Unicode letters)
(randstr "\\p{N}{3}")          ; => "123" (3 Unicode numbers)
(randstr "\\p{P}{2}")          ; => "!@" (2 Unicode punctuation characters)
(randstr "\\p{Lu}{3}\\p{Ll}{3}") ; => "ABCdef" (3 uppercase and 3 lowercase letters)
(randstr "\\p{Letter}{5}")     ; => "abcde" (5 Unicode letters using alias)
(randstr "\\p{Number}{3}")     ; => "123" (3 Unicode numbers using alias)
(randstr "\\p{Script=Han}{2}") ; => "你好" (2 Chinese Han characters)
(randstr "\\p{Block=Basic_Latin}{5}") ; => "ABCDE" (5 characters from Basic Latin block)
(randstr "\\p{Alphabetic}{4}") ; => "abcd" (4 alphabetic characters)
(randstr "\\p{White_Space}{3}") ; => " \t\n" (3 whitespace characters)

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