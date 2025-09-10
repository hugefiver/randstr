#lang scribble/manual

@title{randstr: Random String Generator}
@author{Your Name}

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
]