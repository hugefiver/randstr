#lang racket/base

(require racket/contract
         racket/string
         racket/list
         racket/random
         racket/set)

(provide
 (contract-out
  [random-character (-> char?)]
  [random-word-char (-> char?)]
  [random-whitespace-char (-> char?)]
  [random-non-whitespace-char (-> char?)]
  [random-non-word-char (-> char?)]
  [random-digit-char (-> char?)]
  [random-non-digit-char (-> char?)]
  [random-ref ((listof any/c) . -> . any/c)]
  [vector-random-ref (vector? . -> . any/c)]
  [alphanumeric-chars (-> (listof char?))]
  [alphabetic-chars (-> (listof char?))]
  [numeric-chars (-> (listof char?))]
  [non-word-chars (-> (listof char?))]
  [non-digit-chars (-> (listof char?))]
  [uppercase-chars (-> (listof char?))]
  [lowercase-chars (-> (listof char?))]
  [blank-chars (-> (listof char?))]
  [punctuation-chars (-> (listof char?))]
  [control-chars (-> (listof char?))]
  [printable-chars (-> (listof char?))]
  [graphic-chars (-> (listof char?))]
  [ascii-chars (-> (listof char?))]
  [hex-digit-chars (-> (listof char?))]
  [unicode-property-chars (string? . -> . (listof char?))]
  [char-in-unicode-property? (char? string? . -> . boolean?)]
  [get-unicode-property-cache (-> hash?)]
  [get-unicode-property-ranges (string? . -> . (listof (list/c exact-integer? exact-integer?)))]
  [unicode-property-char-count (string? . -> . exact-integer?)]
  [unicode-property-char-at-index (string? exact-integer? . -> . char?)]
  ;; Validation functions for Unicode properties
  [char-in-general-category? (char? (listof symbol?) . -> . boolean?)]
  [char-hex-digit? (char? . -> . boolean?)]
  [char-ideographic? (char? . -> . boolean?)]
  [char-in-han-script? (char? . -> . boolean?)]
  [char-in-latin-script? (char? . -> . boolean?)]
  [char-in-greek-script? (char? . -> . boolean?)]
  [char-in-cyrillic-script? (char? . -> . boolean?)]
  [char-in-hiragana-script? (char? . -> . boolean?)]
  [char-in-katakana-script? (char? . -> . boolean?)]
  [char-in-basic-latin-block? (char? . -> . boolean?)]
  [char-in-latin-1-supplement-block? (char? . -> . boolean?)]
  [char-in-cjk-unified-ideographs-block? (char? . -> . boolean?)]))

;; Generate a random character
(define (random-character)
  (let ([chars "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"])
    (string-ref chars (random (string-length chars)))))

;; Generate a random word character (alphanumeric + underscore)
(define (random-word-char)
  (let ([chars "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_"])
    (string-ref chars (random (string-length chars)))))

;; Generate a random whitespace character
(define (random-whitespace-char)
  (random-ref '(#\space #\tab #\newline #\return)))

;; Generate a random non-whitespace character
(define (random-non-whitespace-char)
  (random-ref (alphanumeric-chars)))

;; Generate a random non-word character
(define (random-non-word-char)
  (random-ref (non-word-chars)))

;; Generate a random digit character
(define (random-digit-char)
  (random-ref (numeric-chars)))

;; Generate a random non-digit character
(define (random-non-digit-char)
  (random-ref (non-digit-chars)))

;; Get a random element from a list
(define (random-ref lst)
  (list-ref lst (random (length lst))))

;; Get a random element from a vector
(define (vector-random-ref vec)
  (vector-ref vec (random (vector-length vec))))

;; Generate list of alphanumeric characters
(define (alphanumeric-chars)
  (string->list "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"))

;; Generate list of alphabetic characters
(define (alphabetic-chars)
  (string->list "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"))

;; Generate list of numeric characters
(define (numeric-chars)
  (string->list "0123456789"))

;; Generate list of non-word characters (not alphanumeric or underscore)
(define (non-word-chars)
  (string->list "!@#$%^&*()_-+={}[]|\\:;\"'<>?,./`~"))

;; Generate list of non-digit characters
(define (non-digit-chars)
  (string->list "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ!@#$%^&*()_-+={}[]|\\:;\"'<>?,./`~"))

;; Generate list of upper case characters
(define (uppercase-chars)
  (string->list "ABCDEFGHIJKLMNOPQRSTUVWXYZ"))

;; Generate list of lower case characters
(define (lowercase-chars)
  (string->list "abcdefghijklmnopqrstuvwxyz"))

;; Generate list of blank characters (space and tab)
(define (blank-chars)
  (list #\space #\tab))

;; Generate list of punctuation characters
(define (punctuation-chars)
  (string->list "!@#$%^&*()_-+={}[]|\\:;\"'<>?,./`~"))

;; Generate list of control characters (ASCII 0-31)
(define (control-chars)
  (for/list ([i (in-range 0 32)])
    (integer->char i)))

;; Generate list of printable characters (ASCII 32-126)
(define (printable-chars)
  (for/list ([i (in-range 32 127)])
    (integer->char i)))

;; Generate list of graphic characters (printable except space)
(define (graphic-chars)
  (for/list ([i (in-range 33 127)])
    (integer->char i)))

;; Generate list of ASCII characters (0-127)
(define (ascii-chars)
  (for/list ([i (in-range 0 128)])
    (integer->char i)))

;; Generate list of hexadecimal digits
(define (hex-digit-chars)
  (string->list "0123456789ABCDEFabcdef"))

;; Check if a character is a hexadecimal digit
(define (char-hex-digit? c)
  (not (null? (member c '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\A #\B #\C #\D #\E #\F #\a #\b #\c #\d #\e #\f)))))

;; Check if a character belongs to specific general categories
(define (char-in-general-category? char categories)
  (not (null? (member (char-general-category char) categories))))

;; Check if a character is ideographic
(define (char-ideographic? c)
  (or (and (>= (char->integer c) #x4e00) (<= (char->integer c) #x9fff))   ; CJK Unified Ideographs
      (and (>= (char->integer c) #x3400) (<= (char->integer c) #x4dbf))   ; CJK Extension A
      (and (>= (char->integer c) #x20000) (<= (char->integer c) #x2a6df)) ; CJK Extension B
      (and (>= (char->integer c) #xf900) (<= (char->integer c) #xfaff)))) ; CJK Compatibility Ideographs

;; Check if a character belongs to Han script
(define (char-in-han-script? c)
  (or (and (>= (char->integer c) #x4e00) (<= (char->integer c) #x9fff))   ; CJK Unified Ideographs
      (and (>= (char->integer c) #x3400) (<= (char->integer c) #x4dbf))   ; CJK Extension A
      (and (>= (char->integer c) #x20000) (<= (char->integer c) #x2a6df)) ; CJK Extension B
      (and (>= (char->integer c) #xf900) (<= (char->integer c) #xfaff)))) ; CJK Compatibility Ideographs

;; Check if a character belongs to Latin script
(define (char-in-latin-script? c)
  (let ([cat (char-general-category c)])
    (and (member cat '(lu ll lt lm lo))
         (or (<= (char->integer c) #x24f)   ; Basic Latin + Latin-1 Supplement + Latin Extended-A
             (and (>= (char->integer c) #x1e00) (<= (char->integer c) #x1eff))))))

;; Check if a character belongs to Greek script
(define (char-in-greek-script? c)
  (and (>= (char->integer c) #x370) (<= (char->integer c) #x3ff)))

;; Check if a character belongs to Cyrillic script
(define (char-in-cyrillic-script? c)
  (and (>= (char->integer c) #x400) (<= (char->integer c) #x4ff)))

;; Check if a character belongs to Hiragana script
(define (char-in-hiragana-script? c)
  (and (>= (char->integer c) #x3040) (<= (char->integer c) #x309f)))

;; Check if a character belongs to Katakana script
(define (char-in-katakana-script? c)
  (and (>= (char->integer c) #x30a0) (<= (char->integer c) #x30ff)))

;; Check if a character belongs to Basic Latin block
(define (char-in-basic-latin-block? c)
  (<= (char->integer c) #x7f))

;; Check if a character belongs to Latin-1 Supplement block
(define (char-in-latin-1-supplement-block? c)
  (and (>= (char->integer c) #x80) (<= (char->integer c) #xff)))

;; Check if a character belongs to CJK Unified Ideographs block
(define (char-in-cjk-unified-ideographs-block? c)
  (and (>= (char->integer c) #x4e00) (<= (char->integer c) #x9fff)))

;; Define Unicode ranges for efficient character handling
;; Each range is a list of (start end) pairs
(define unicode-ranges
  (list
    (list 0 #xd800)    ; Characters before surrogate range
    (list #xe000 #xffff))) ; Characters after surrogate range

;; Get ranges for a Unicode property (for efficient random access)
(define (get-unicode-property-ranges property)
  (let ([normalized-prop (normalize-unicode-property-name property)])
    (cond
      ; General categories - would need to map to ranges, simplified for now
      [(member normalized-prop '("L" "Letter"))
       (list (list #x4e00 #x9fff) (list #x3400 #x4dbf) (list #x20000 #x2a6df) (list #xf900 #xfaff) ; Han
             (list #x0041 #x005a) (list #x0061 #x007a) (list #x00c0 #x00d6) (list #x00d8 #x00f6) (list #x00f8 #x00ff) ; Latin
             (list #x0100 #x017f) (list #x0180 #x024f) (list #x1e00 #x1eff))] ; Extended Latin
      [(member normalized-prop '("N" "Number"))
       (append (list (list #x0030 #x0039) (list #x00b2 #x00b3) (list #x00b9 #x00b9) (list #x00bc #x00be)) ; Basic digits
               (list (list #x2070 #x2079) (list #x2080 #x2089) (list #x2150 #x218f) (list #x2460 #x24ff)) ; Superscript/subscript and fractions
               (list (list #x2776 #x2793) (list #x2cfd #x2cfd) (list #x2d30 #x2d6f) (list #x2e2f #x2e2f)) ; Circled and parenthesized
               (list (list #x3007 #x3007) (list #x3021 #x3029) (list #x3038 #x303a) (list #x3192 #x3195)) ; CJK ideographic
               (list (list #x3220 #x3229) (list #x3250 #x325f) (list #x3280 #x3289) (list #x32b1 #x32bf)) ; Circled
               (list (list #xa620 #xa629) (list #xa6e6 #xa6ef) (list #xa830 #xa835) (list #xa8d0 #xa8d9)) ; Others
               (list (list #xa900 #xa909) (list #xa9d0 #xa9d9) (list #xaa50 #xaa59) (list #xabf0 #xabf9)))] ; More
      ; Binary properties - would need to map to ranges, simplified for now
      [(member normalized-prop '("Alphabetic"))
       (list (list #x0041 #x005a) (list #x0061 #x007a) (list #x00c0 #x00d6) (list #x00d8 #x00f6) (list #x00f8 #x00ff) ; Latin
             (list #x0100 #x017f) (list #x0180 #x024f) (list #x1e00 #x1eff))] ; Extended Latin
      [(member normalized-prop '("White_Space"))
       (list (list #x0009 #x000d) (list #x0020 #x0020) (list #x0085 #x0085) (list #x00a0 #x00a0) ; Basic whitespace
             (list #x1680 #x1680) (list #x2000 #x200a) (list #x2028 #x2029) (list #x202f #x202f) ; More whitespace
             (list #x205f #x205f) (list #x3000 #x3000))] ; Final whitespace
      ; Script properties
      [(or (string-prefix? normalized-prop "Script=")
          (member normalized-prop '("Han" "Hani")))
       (let ([script-name (if (string-prefix? normalized-prop "Script=")
                             (substring normalized-prop 7)
                             normalized-prop)])
         (case script-name
           [("Han" "Hani")
            (list (list #x4e00 #x9fff) (list #x3400 #x4dbf) (list #x20000 #x2a6df) (list #xf900 #xfaff))]
           [("Latin" "Latn")
            (list (list #x0000 #x024f) (list #x1e00 #x1eff))]
           [("Greek" "Grek")
            (list (list #x0370 #x03ff))]
           [("Cyrillic" "Cyrl")
            (list (list #x0400 #x04ff))]
           [("Hiragana" "Hira")
            (list (list #x3040 #x309f))]
           [("Katakana" "Kana")
            (list (list #x30a0 #x30ff))]
           [else '()]))]
      ; Block properties
      [(string-prefix? normalized-prop "Block=")
       (let ([block-name (substring normalized-prop 6)])
         (case block-name
           [("Basic_Latin")
            (list (list 0 #x7f))]
           [("Latin-1_Supplement")
            (list (list #x80 #xff))]
           [("CJK_Unified_Ideographs")
            (list (list #x4e00 #x9fff))]
           [else '()]))]
      [else '()])))

;; Get total character count for a Unicode property
(define (unicode-property-char-count property)
  (let ([ranges (get-unicode-property-ranges property)])
    (apply + (map (lambda (range) (+ 1 (- (cadr range) (car range)))) ranges))))

;; Get character at specific index in a Unicode property
(define (unicode-property-char-at-index property index)
  (let ([ranges (get-unicode-property-ranges property)])
    (let loop ([ranges ranges]
               [current-index 0])
      (if (null? ranges)
          #f  ; Index out of bounds
          (let* ([range (car ranges)]
                 [start (car range)]
                 [end (cadr range)]
                 [range-size (+ 1 (- end start))])
            (if (<= index (+ current-index range-size -1))
                (integer->char (+ start (- index current-index)))
                (loop (cdr ranges) (+ current-index range-size))))))))

;; Unicode property cache
(define unicode-property-cache (make-hash))

;; Check if a character belongs to a Unicode property
(define (char-in-unicode-property? char property)
  (let ([ranges (get-unicode-property-ranges property)])
    (let ([char-int (char->integer char)])
      (ormap (lambda (range)
               (and (>= char-int (car range)) (<= char-int (cadr range))))
             ranges))))

;; Get the Unicode property cache
(define (get-unicode-property-cache)
  unicode-property-cache)

;; Get characters for a Unicode property (with caching)
(define (unicode-property-chars property)
  (cond
    [(hash-has-key? unicode-property-cache property)
     (hash-ref unicode-property-cache property)]
    [else
     (let ([chars (generate-unicode-property-chars-from-ranges property)])
       (hash-set! unicode-property-cache property chars)
       chars)]))

;; Generate characters for a Unicode property from ranges (for memory efficiency)
(define (generate-unicode-property-chars-from-ranges property)
  (let ([ranges (get-unicode-property-ranges property)])
    (apply append
           (map (lambda (range)
                  (let ([start (car range)]
                        [end (cadr range)])
                    (for/list ([i (in-range start (+ end 1))])
                      (integer->char i))))
                ranges))))

;; Generate characters for a Unicode property (using Racket's Unicode functions)
(define (generate-unicode-property-chars property)
  (let ([normalized-prop (normalize-unicode-property-name property)])
    (cond
      ; General categories
      [(member normalized-prop '("L" "Letter"))
       (filter-unicode-by-category '(lu ll lt lm lo))]
      [(member normalized-prop '("Lu" "Uppercase_Letter"))
       (filter (lambda (c) (eq? (char-general-category c) 'lu)) (get-all-unicode-chars))]
      [(member normalized-prop '("Ll" "Lowercase_Letter"))
       (filter (lambda (c) (eq? (char-general-category c) 'll)) (get-all-unicode-chars))]
      [(member normalized-prop '("Lt" "Titlecase_Letter"))
       (filter (lambda (c) (eq? (char-general-category c) 'lt)) (get-all-unicode-chars))]
      [(member normalized-prop '("Lm" "Modifier_Letter"))
       (filter (lambda (c) (eq? (char-general-category c) 'lm)) (get-all-unicode-chars))]
      [(member normalized-prop '("Lo" "Other_Letter"))
       (filter (lambda (c) (eq? (char-general-category c) 'lo)) (get-all-unicode-chars))]
      [(member normalized-prop '("N" "Number"))
       (filter-unicode-by-category '(nd nl no))]
      [(member normalized-prop '("Nd" "Decimal_Number"))
       (filter (lambda (c) (eq? (char-general-category c) 'nd)) (get-all-unicode-chars))]
      [(member normalized-prop '("Nl" "Letter_Number"))
       (filter (lambda (c) (eq? (char-general-category c) 'nl)) (get-all-unicode-chars))]
      [(member normalized-prop '("No" "Other_Number"))
       (filter (lambda (c) (eq? (char-general-category c) 'no)) (get-all-unicode-chars))]
      [(member normalized-prop '("P" "Punctuation"))
       (filter-unicode-by-category '(pc pd ps pe pi pf po))]
      [(member normalized-prop '("Pc" "Connector_Punctuation"))
       (filter (lambda (c) (eq? (char-general-category c) 'pc)) (get-all-unicode-chars))]
      [(member normalized-prop '("Pd" "Dash_Punctuation"))
       (filter (lambda (c) (eq? (char-general-category c) 'pd)) (get-all-unicode-chars))]
      [(member normalized-prop '("Ps" "Open_Punctuation"))
       (filter (lambda (c) (eq? (char-general-category c) 'ps)) (get-all-unicode-chars))]
      [(member normalized-prop '("Pe" "Close_Punctuation"))
       (filter (lambda (c) (eq? (char-general-category c) 'pe)) (get-all-unicode-chars))]
      [(member normalized-prop '("Pi" "Initial_Punctuation"))
       (filter (lambda (c) (eq? (char-general-category c) 'pi)) (get-all-unicode-chars))]
      [(member normalized-prop '("Pf" "Final_Punctuation"))
       (filter (lambda (c) (eq? (char-general-category c) 'pf)) (get-all-unicode-chars))]
      [(member normalized-prop '("Po" "Other_Punctuation"))
       (filter (lambda (c) (eq? (char-general-category c) 'po)) (get-all-unicode-chars))]
      [(member normalized-prop '("M" "Mark"))
       (filter-unicode-by-category '(mn mc me))]
      [(member normalized-prop '("Mn" "Nonspacing_Mark"))
       (filter (lambda (c) (eq? (char-general-category c) 'mn)) (get-all-unicode-chars))]
      [(member normalized-prop '("Mc" "Spacing_Mark"))
       (filter (lambda (c) (eq? (char-general-category c) 'mc)) (get-all-unicode-chars))]
      [(member normalized-prop '("Me" "Enclosing_Mark"))
       (filter (lambda (c) (eq? (char-general-category c) 'me)) (get-all-unicode-chars))]
      [(member normalized-prop '("Z" "Separator"))
       (filter-unicode-by-category '(zs zl zp))]
      [(member normalized-prop '("Zs" "Space_Separator"))
       (filter (lambda (c) (eq? (char-general-category c) 'zs)) (get-all-unicode-chars))]
      [(member normalized-prop '("Zl" "Line_Separator"))
       (filter (lambda (c) (eq? (char-general-category c) 'zl)) (get-all-unicode-chars))]
      [(member normalized-prop '("Zp" "Paragraph_Separator"))
       (filter (lambda (c) (eq? (char-general-category c) 'zp)) (get-all-unicode-chars))]
      [(member normalized-prop '("S" "Symbol"))
       (filter-unicode-by-category '(sm sc sk so))]
      [(member normalized-prop '("Sm" "Math_Symbol"))
       (filter (lambda (c) (eq? (char-general-category c) 'sm)) (get-all-unicode-chars))]
      [(member normalized-prop '("Sc" "Currency_Symbol"))
       (filter (lambda (c) (eq? (char-general-category c) 'sc)) (get-all-unicode-chars))]
      [(member normalized-prop '("Sk" "Modifier_Symbol"))
       (filter (lambda (c) (eq? (char-general-category c) 'sk)) (get-all-unicode-chars))]
      [(member normalized-prop '("So" "Other_Symbol"))
       (filter (lambda (c) (eq? (char-general-category c) 'so)) (get-all-unicode-chars))]
      [(member normalized-prop '("C" "Other"))
       (filter-unicode-by-category '(cc cf cs co cn))]
      [(member normalized-prop '("Cc" "Control"))
       (filter (lambda (c) (eq? (char-general-category c) 'cc)) (get-all-unicode-chars))]
      [(member normalized-prop '("Cf" "Format"))
       (filter (lambda (c) (eq? (char-general-category c) 'cf)) (get-all-unicode-chars))]
      [(member normalized-prop '("Cs" "Surrogate"))
       (filter (lambda (c) (eq? (char-general-category c) 'cs)) (get-all-unicode-chars))]
      [(member normalized-prop '("Co" "Private_Use"))
       (filter (lambda (c) (eq? (char-general-category c) 'co)) (get-all-unicode-chars))]
      [(member normalized-prop '("Cn" "Unassigned"))
       (filter (lambda (c) (eq? (char-general-category c) 'cn)) (get-all-unicode-chars))]
      ; Binary properties
      [(member normalized-prop '("Alphabetic"))
       (filter-unicode-by-predicate char-alphabetic?)]
      [(member normalized-prop '("Uppercase"))
       (filter-unicode-by-predicate char-upper-case?)]
      [(member normalized-prop '("Lowercase"))
       (filter-unicode-by-predicate char-lower-case?)]
      [(member normalized-prop '("White_Space"))
       (filter-unicode-by-predicate char-whitespace?)]
      [(member normalized-prop '("Hex_Digit"))
       (filter-unicode-by-predicate char-hex-digit?)]
      [(member normalized-prop '("Ideographic"))
       ; For Ideographic, we'll use a combination of criteria
       (filter (lambda (c)
                 (or (and (>= (char->integer c) #x4e00) (<= (char->integer c) #x9fff))  ; CJK Unified Ideographs
                     (and (>= (char->integer c) #x3400) (<= (char->integer c) #x4dbf))  ; CJK Extension A
                     (and (>= (char->integer c) #x20000) (<= (char->integer c) #x2a6df)) ; CJK Extension B
                     (and (>= (char->integer c) #xf900) (<= (char->integer c) #xfaff))))  ; CJK Compatibility Ideographs
               (get-all-unicode-chars))]
      [(member normalized-prop '("ASCII"))
       (filter-unicode-by-range 0 127)]
      [(member normalized-prop '("Any"))
       (get-all-unicode-chars)]
      ; Script properties
      [(string-prefix? normalized-prop "Script=")
       (let ([script-name (substring normalized-prop 7)])
         (get-script-chars script-name))]
      ; Short script names (e.g., Han, Latin, Greek)
      [(member normalized-prop '("Han" "Hani"))
       (get-script-chars "Han")]
      [(member normalized-prop '("Latin" "Latn"))
       (get-script-chars "Latin")]
      [(member normalized-prop '("Greek" "Grek"))
       (get-script-chars "Greek")]
      [(member normalized-prop '("Cyrillic" "Cyrl"))
       (get-script-chars "Cyrillic")]
      [(member normalized-prop '("Hiragana" "Hira"))
       (get-script-chars "Hiragana")]
      [(member normalized-prop '("Katakana" "Kana"))
       (get-script-chars "Katakana")]
      ; Block properties
      [(string-prefix? normalized-prop "Block=")
       (let ([block-name (substring normalized-prop 6)])
         (get-block-chars block-name))]
      [else (error "Invalid Unicode property: " property)])))

;; Normalize Unicode property name (convert aliases to standard form)
(define (normalize-unicode-property-name property)
  (cond
    [(string-contains? property "=") property] ; For key-value properties like Script=Han, Block=Basic_Latin
    [else
     (case property
       [("Letter") "L"]
       [("Uppercase_Letter") "Lu"]
       [("Lowercase_Letter") "Ll"]
       [("Titlecase_Letter") "Lt"]
       [("Modifier_Letter") "Lm"]
       [("Other_Letter") "Lo"]
       [("Number") "N"]
       [("Decimal_Number") "Nd"]
       [("Letter_Number") "Nl"]
       [("Other_Number") "No"]
       [("Punctuation") "P"]
       [("Connector_Punctuation") "Pc"]
       [("Dash_Punctuation") "Pd"]
       [("Open_Punctuation") "Ps"]
       [("Close_Punctuation") "Pe"]
       [("Initial_Punctuation") "Pi"]
       [("Final_Punctuation") "Pf"]
       [("Other_Punctuation") "Po"]
       [("Mark") "M"]
       [("Nonspacing_Mark") "Mn"]
       [("Spacing_Mark") "Mc"]
       [("Enclosing_Mark") "Me"]
       [("Separator") "Z"]
       [("Space_Separator") "Zs"]
       [("Line_Separator") "Zl"]
       [("Paragraph_Separator") "Zp"]
       [("Symbol") "S"]
       [("Math_Symbol") "Sm"]
       [("Currency_Symbol") "Sc"]
       [("Modifier_Symbol") "Sk"]
       [("Other_Symbol") "So"]
       [("Other") "C"]
       [("Control") "Cc"]
       [("Format") "Cf"]
       [("Surrogate") "Cs"]
       [("Private_Use") "Co"]
       [("Unassigned") "Cn"]
       [("Alphabetic") "Alphabetic"]
       [("Uppercase") "Uppercase"]
       [("Lowercase") "Lowercase"]
       [("White_Space") "White_Space"]
       [("Hex_Digit") "Hex_Digit"]
       [("Ideographic") "Ideographic"]
       [("ASCII") "ASCII"]
       [("Any") "Any"]
       [else property])]))

;; Filter Unicode characters by category
(define (filter-unicode-by-category categories)
  (for*/list ([range unicode-ranges]
              [i (in-range (car range) (cadr range))]
              [char (list (integer->char i))]
              #:when (member (char-general-category char) categories))
    char))

;; Filter Unicode characters by predicate
(define (filter-unicode-by-predicate pred)
  (for*/list ([range unicode-ranges]
              [i (in-range (car range) (cadr range))]
              [char (list (integer->char i))]
              #:when (pred char))
    char))

;; Filter Unicode characters by range
(define (filter-unicode-by-range start end)
  (for*/list ([range unicode-ranges]
              [i (in-range (car range) (cadr range))]
              #:when (and (>= i start) (<= i end)))
    (integer->char i)))

;; Get all Unicode characters
(define (get-all-unicode-chars)
  (for*/list ([range unicode-ranges]
              [i (in-range (car range) (cadr range))])
    (integer->char i)))

;; Get characters for a specific script
(define (get-script-chars script-name)
  (case script-name
    [("Han" "Hani")
     (append
       (for/list ([i (in-range #x4e00 (+ #x9fff 1))]) (integer->char i))
       (for/list ([i (in-range #x3400 (+ #x4dbf 1))]) (integer->char i))
       (for/list ([i (in-range #x20000 (+ #x2a6df 1))]) (integer->char i))
       (for/list ([i (in-range #xf900 (+ #xfaff 1))]) (integer->char i)))]
    [("Latin" "Latn")
     (for*/list ([range unicode-ranges]
                 [i (in-range (car range) (+ (cadr range) 1))]
                 [char (list (integer->char i))]
                 #:when (let ([cat (char-general-category char)])
                          (and (member cat '(lu ll lt lm lo))
                               (or (<= (char->integer char) #x24f)  ; Basic Latin + Latin-1 Supplement + Latin Extended-A
                                   (and (>= (char->integer char) #x1e00) (<= (char->integer char) #x1eff))))))
       char)]
    [("Greek" "Grek")
     (for/list ([i (in-range #x370 (+ #x3ff 1))]) (integer->char i))]
    [("Cyrillic" "Cyrl")
     (for/list ([i (in-range #x400 (+ #x4ff 1))]) (integer->char i))]
    [("Hiragana" "Hira")
     (for/list ([i (in-range #x3040 (+ #x309f 1))]) (integer->char i))]
    [("Katakana" "Kana")
     (for/list ([i (in-range #x30a0 (+ #x30ff 1))]) (integer->char i))]
    [else '()]))

;; Get characters for a specific block
(define (get-block-chars block-name)
  (case block-name
    [("Basic_Latin")
     (for/list ([i (in-range 0 (+ #x7f 1))]) (integer->char i))]
    [("Latin-1_Supplement")
     (for/list ([i (in-range #x80 (+ #xff 1))]) (integer->char i))]
    [("CJK_Unified_Ideographs")
     (for/list ([i (in-range #x4e00 (+ #x9fff 1))]) (integer->char i))]
    [else '()]))