#lang racket/base

(require rackunit
         racket/list
         "../char-classes.rkt")

;; Test cases for char-classes module

(test-case "random-character: generates valid character"
  (let ([char (random-character)])
    (check-true (char? char))
    (check-not-false (member char (printable-chars)))))

(test-case "random-word-char: generates valid word character"
  (let ([char (random-word-char)])
    (check-true (char? char))
    (check-true (or (char-alphabetic? char)
                    (char-numeric? char)
                    (char=? char #\_)))))

(test-case "random-whitespace-char: generates valid whitespace character"
  (let ([char (random-whitespace-char)])
    (check-true (char? char))
    (check-true (or (char=? char #\space)
                    (char=? char #\tab)
                    (char=? char #\newline)
                    (char=? char #\return)))))

(test-case "random-non-whitespace-char: generates printable non-whitespace"
  (let ([char (random-non-whitespace-char)])
    (check-true (char? char))
    (check-not-false (member char (printable-chars)))
    (check-false (member char (list #\space #\tab #\newline #\return)))))

(test-case "random-digit-char: generates valid digit character"
  (let ([char (random-digit-char)])
    (check-true (char? char))
    (check-true (char-numeric? char))))

(test-case "random-ref: returns element from list"
  (let ([lst '(a b c d e)])
    (let ([result (random-ref lst)])
      (check-not-false (member result lst)))))

(test-case "character lists: contain expected characters"
  (let ([alpha-chars (alphabetic-chars)])
    (check-not-false (member #\a alpha-chars))
    (check-not-false (member #\Z alpha-chars)))
  (let ([num-chars (numeric-chars)])
    (check-not-false (member #\0 num-chars))
    (check-not-false (member #\9 num-chars)))
  (let ([upper-chars (uppercase-chars)])
    (check-not-false (member #\A upper-chars)))
  (let ([lower-chars (lowercase-chars)])
    (check-not-false (member #\z lower-chars)))
  (let ([blank-chars (blank-chars)])
    (check-not-false (member #\space blank-chars))
    (check-not-false (member #\tab blank-chars))))

(test-case "non-word-chars: contains expected non-word characters"
  (let ([non-word (non-word-chars)])
    (check-not-false (member #\! non-word))
    (check-not-false (member #\@ non-word))
    (check-not-false (member #\# non-word))))

(test-case "hex-digit-chars: contains all hex digits"
  (let ([hex-chars (hex-digit-chars)])
    (check-not-false (member #\0 hex-chars))
    (check-not-false (member #\9 hex-chars))
    (check-not-false (member #\A hex-chars))
    (check-not-false (member #\F hex-chars))
    (check-not-false (member #\a hex-chars))
    (check-not-false (member #\f hex-chars))))

(test-case "unicode-property-chars: returns valid character lists"
  (let ([letter-chars (unicode-property-chars "L")])
    (check-true (list? letter-chars))
    (check-true (> (length letter-chars) 0))
    (let ([sample-size (min 10 (length letter-chars))])
      (for ([char (take letter-chars sample-size)])
        (check-true (char-alphabetic? char)))))
  (let ([digit-chars (unicode-property-chars "N")])
    (check-true (list? digit-chars))
    (check-true (> (length digit-chars) 0))
    (let ([sample-size (min 10 (length digit-chars))])
      (for ([char (take digit-chars sample-size)])
        (check-true (char-numeric? char)))))
  (let ([punct-chars (unicode-property-chars "P")])
    (check-true (list? punct-chars))
    (check-true (> (length punct-chars) 0))
    (let ([sample-size (min 10 (length punct-chars))])
      (for ([char (take punct-chars sample-size)])
        (check-true (char-punctuation? char)))))
  (let ([upper-chars (unicode-property-chars "Lu")])
    (check-true (list? upper-chars))
    (check-true (> (length upper-chars) 0))
    (let ([sample-size (min 10 (length upper-chars))])
      (for ([char (take upper-chars sample-size)])
        (check-true (char-upper-case? char)))))
  (let ([lower-chars (unicode-property-chars "Ll")])
    (check-true (list? lower-chars))
    (check-true (> (length lower-chars) 0))
    (let ([sample-size (min 10 (length lower-chars))])
      (for ([char (take lower-chars sample-size)])
        (check-true (char-lower-case? char))))))

(test-case "unicode-property-chars: handles invalid property names"
  (check-exn exn:fail? (lambda () (unicode-property-chars "InvalidProperty"))))

(test-case "unicode-property-chars: script properties"
  (let ([han-chars (unicode-property-chars "Script=Han")])
    (check-true (list? han-chars))
    (check-true (> (length han-chars) 0))
    (let ([sample-size (min 5 (length han-chars))])
      (for ([char (take han-chars sample-size)])
        (check-true (char-in-han-script? char)))))
  (let ([latin-chars (unicode-property-chars "Script=Latin")])
    (check-true (list? latin-chars))
    (check-true (> (length latin-chars) 0))
    (let ([sample-size (min 5 (length latin-chars))])
      (for ([char (take latin-chars sample-size)])
        (check-true (char-in-latin-script? char))))))

(test-case "unicode-property-chars: block properties"
  (let ([basic-latin-chars (unicode-property-chars "Block=Basic_Latin")])
    (check-true (list? basic-latin-chars))
    (check-true (> (length basic-latin-chars) 0))
    (let ([sample-size (min 5 (length basic-latin-chars))])
      (for ([char (take basic-latin-chars sample-size)])
        (check-true (char-in-basic-latin-block? char)))))
  (let ([cjk-unified-chars (unicode-property-chars "Block=CJK_Unified_Ideographs")])
    (check-true (list? cjk-unified-chars))
    (check-true (> (length cjk-unified-chars) 0))
    (let ([sample-size (min 5 (length cjk-unified-chars))])
      (for ([char (take cjk-unified-chars sample-size)])
        (check-true (char-in-cjk-unified-ideographs-block? char))))))

(test-case "unicode-property-chars: binary properties"
  (let ([alphabetic-chars (unicode-property-chars "Alphabetic")])
    (check-true (list? alphabetic-chars))
    (check-true (> (length alphabetic-chars) 0))
    (let ([sample-size (min 5 (length alphabetic-chars))])
      (for ([char (take alphabetic-chars sample-size)])
        (check-true (char-alphabetic? char)))))
  (let ([uppercase-chars (unicode-property-chars "Uppercase")])
    (check-true (list? uppercase-chars))
    (check-true (> (length uppercase-chars) 0))
    (let ([sample-size (min 5 (length uppercase-chars))])
      (for ([char (take uppercase-chars sample-size)])
        (check-true (char-upper-case? char)))))
  (let ([lowercase-chars (unicode-property-chars "Lowercase")])
    (check-true (list? lowercase-chars))
    (check-true (> (length lowercase-chars) 0))
    (let ([sample-size (min 5 (length lowercase-chars))])
      (for ([char (take lowercase-chars sample-size)])
        (check-true (char-lower-case? char)))))
  (let ([whitespace-chars (unicode-property-chars "White_Space")])
    (check-true (list? whitespace-chars))
    (check-true (> (length whitespace-chars) 0))
    (let ([sample-size (min 5 (length whitespace-chars))])
      (for ([char (take whitespace-chars sample-size)])
        (check-true (char-whitespace? char)))))
  (let ([hex-digit-chars (unicode-property-chars "Hex_Digit")])
    (check-true (list? hex-digit-chars))
    (check-true (> (length hex-digit-chars) 0))
    (let ([sample-size (min 5 (length hex-digit-chars))])
      (for ([char (take hex-digit-chars sample-size)])
        (check-true (char-hex-digit? char)))))
  (let ([ideographic-chars (unicode-property-chars "Ideographic")])
    (check-true (list? ideographic-chars))
    (check-true (> (length ideographic-chars) 0))
    (let ([sample-size (min 5 (length ideographic-chars))])
      (for ([char (take ideographic-chars sample-size)])
        (check-true (char-ideographic? char))))))

(test-case "unicode-property-chars: additional binary properties"
  (let ([cased-chars (unicode-property-chars "Cased")])
    (check-true (list? cased-chars))
    (check-true (> (length cased-chars) 0))
    (let ([sample-size (min 5 (length cased-chars))])
      (for ([char (take cased-chars sample-size)])
        (check-true (char-cased? char)))))
  (let ([dash-chars (unicode-property-chars "Dash")])
    (check-true (list? dash-chars))
    (check-true (> (length dash-chars) 0))
    ;; Check that some known dash characters are included
    (check-not-false (member #\- dash-chars))  ; Hyphen-minus
    (check-not-false (member #\— dash-chars))  ; Em dash
    (check-not-false (member #\– dash-chars)))) ; En dash
  (let ([emoji-chars (unicode-property-chars "Emoji")])
    (check-true (list? emoji-chars))
    (check-true (> (length emoji-chars) 0))
    ;; Check that some known emoji are included (these are in the basic range)
    (check-not-false (member (integer->char #x1F600) emoji-chars))  ; Grinning face
    (check-not-false (member (integer->char #x1F601) emoji-chars))  ; Grinning face with smiling eyes
    ;; Check Unicode 15.0 additions
    (check-not-false (member (integer->char #x1FA70) emoji-chars))  ; First alchemical symbol
    (check-not-false (member (integer->char #x1FA73) emoji-chars))  ; Last alchemical symbol
    (check-not-false (member (integer->char #x1FA80) emoji-chars))  ; First chess symbol
    (check-not-false (member (integer->char #x1FA86) emoji-chars))) ; Last chess symbol
  (let ([emoji-component-chars (unicode-property-chars "Emoji_Component")])
    (check-true (list? emoji-component-chars))
    (check-true (> (length emoji-component-chars) 0))
    ;; Check that some known emoji components are included
    (check-not-false (member (integer->char #x200D) emoji-component-chars))  ; Zero Width Joiner
    (check-not-false (member (integer->char #xFE0F) emoji-component-chars))  ; Variation Selector-16
    (check-not-false (member (integer->char #x1F3FB) emoji-component-chars)))  ; Skin tone modifier
  (let ([emoji-modifier-chars (unicode-property-chars "Emoji_Modifier")])
    (check-true (list? emoji-modifier-chars))
    (check-true (> (length emoji-modifier-chars) 0))
    ;; Check that skin tone modifiers are included
    (check-not-false (member (integer->char #x1F3FB) emoji-modifier-chars))  ; Light skin tone
    (check-not-false (member (integer->char #x1F3FF) emoji-modifier-chars))) ; Dark skin tone
  (let ([id-continue-chars (unicode-property-chars "ID_Continue")])
    (check-true (list? id-continue-chars))
    (check-true (> (length id-continue-chars) 0))
    ;; Check that some known ID_Continue characters are included
    (check-not-false (member #\a id-continue-chars))
    (check-not-false (member #\0 id-continue-chars))
    (check-not-false (member #\_ id-continue-chars)))
  (let ([id-start-chars (unicode-property-chars "ID_Start")])
    (check-true (list? id-start-chars))
    (check-true (> (length id-start-chars) 0))
    ;; Check that some known ID_Start characters are included
    (check-not-false (member #\a id-start-chars))
    (check-not-false (member #\A id-start-chars)))

;; Run tests
(void)
