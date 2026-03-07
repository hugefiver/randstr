-- | Tokenizer for parsing regex-like patterns into tokens.
module Randstr.Tokenizer
  ( tokenizePattern
  , parseCharacterClass
  , parseQuantifier
  , parseGroup
  , parseUnicodeProperty
  , rangeToList
  ) where

import Randstr.Types
import Randstr.CharClasses
import Randstr.Utils (removeDuplicatesPreservingOrder)

-- | Tokenize a pattern string into a list of tokens.
tokenizePattern :: String -> [Token]
tokenizePattern = reverse . go []
  where
    go :: [Token] -> String -> [Token]
    go tokens [] = tokens

    -- Start anchor - skip
    go tokens ('^':rest) = go tokens rest

    -- End anchor - skip
    go tokens ('$':rest) = go tokens rest

    -- Character class
    go tokens ('[':rest) =
      let (options, remaining) = parseCharacterClass rest
      in go (Token CharClass (CharsContent options) NoQuantifier : tokens) remaining

    -- Group - check for named group
    go tokens ('(':rest) = case rest of
      ('?':'<':more) ->
        let (name, groupContent, remaining) = parseNamedGroup more
        in go (Token NamedGroup (NamedGroupContent name groupContent) NoQuantifier : tokens) remaining
      _ ->
        let (groupContent, remaining) = parseGroupInternal rest 1
        in go (Token Group (StringContent groupContent) NoQuantifier : tokens) remaining

    -- Brace quantifier
    go [] ('{':rest) = go [] rest
    go (lastToken:restTokens) ('{':rest) =
      let (quant, remaining) = parseQuantifier rest
      in go (lastToken { tokenQuantifier = quant } : restTokens) remaining

    -- Star quantifier
    go [] ('*':rest) = go [] rest
    go (lastToken:restTokens) ('*':rest) =
      go (lastToken { tokenQuantifier = Star } : restTokens) rest

    -- Plus quantifier
    go [] ('+':rest) = go [] rest
    go (lastToken:restTokens) ('+':rest) =
      go (lastToken { tokenQuantifier = Plus } : restTokens) rest

    -- Optional quantifier
    go [] ('?':rest) = go [] rest
    go (lastToken:restTokens) ('?':rest) =
      go (lastToken { tokenQuantifier = Optional } : restTokens) rest

    -- Any character
    go tokens ('.':rest) =
      go (Token AnyChar NoContent NoQuantifier : tokens) rest

    -- Escape sequences
    go tokens ('\\':c:rest) = case c of
      'w' -> go (Token WordChar NoContent NoQuantifier : tokens) rest
      'W' -> go (Token NonWordChar NoContent NoQuantifier : tokens) rest
      's' -> go (Token WhitespaceChar NoContent NoQuantifier : tokens) rest
      'S' -> go (Token NonWhitespaceChar NoContent NoQuantifier : tokens) rest
      'd' -> go (Token DigitChar NoContent NoQuantifier : tokens) rest
      'D' -> go (Token NonDigitChar NoContent NoQuantifier : tokens) rest
      'p' -> case rest of
        ('{':more) ->
          let (property, remaining) = parseUnicodeProperty more
          in go (Token UnicodeProperty (StringContent property) NoQuantifier : tokens) remaining
        _ -> go (Token Literal (CharContent c) NoQuantifier : tokens) rest
      'k' -> case rest of
        ('<':more) ->
          let (name, remaining) = parseBackreferenceName more
          in go (Token Backreference (StringContent name) NoQuantifier : tokens) remaining
        _ -> go (Token Literal (CharContent c) NoQuantifier : tokens) rest
      _ -> go (Token Literal (CharContent c) NoQuantifier : tokens) rest

    -- Literal character
    go tokens (c:rest) =
      go (Token Literal (CharContent c) NoQuantifier : tokens) rest

-- | Parse a character class like [abc] or [a-z].
-- Takes the characters after the opening '['.
-- Returns (list of unique characters, remaining characters).
parseCharacterClass :: String -> ([Char], String)
parseCharacterClass [] = ([], [])
parseCharacterClass ('^':rest) =
  let (options, remaining) = parseClassBody rest [] False Nothing
      uniqueOptions = removeDuplicatesPreservingOrder (reverse options)
      finalOptions = filter (`notElem` uniqueOptions) printableChars
  in (finalOptions, remaining)
parseCharacterClass chars =
  let (options, remaining) = parseClassBody chars [] False Nothing
      uniqueOptions = removeDuplicatesPreservingOrder (reverse options)
  in (uniqueOptions, remaining)

-- | Parse the body of a character class.
parseClassBody :: String -> [Char] -> Bool -> Maybe Char -> ([Char], String)
-- End of input
parseClassBody [] options _ _ = (options, [])
-- Handle nested POSIX character classes like [[:alpha:][:digit:]]
parseClassBody ('[':':':rest) options _ _ =
  let (posixChars, newRemaining) = parsePosixCharacterClass rest
  in parseClassBody newRemaining (posixChars ++ options) False Nothing
-- Closing bracket
parseClassBody (']':rest) options _ _
  | null options = ([']'], rest)  -- Empty class, treat ] as literal
  | otherwise = (options, rest)
-- Escape sequences inside character classes
parseClassBody ('\\':c:rest) options _ _ =
  let expanded = classEscapeToChars c
  in parseClassBody rest (reverse expanded ++ options) False Nothing
-- Range with valid start
parseClassBody (c:rest) options True (Just rangeStart)
  | rangeStart <= c = parseClassBody rest (rangeToList rangeStart c ++ options) False Nothing
  | otherwise = parseClassBody rest (c : options) False Nothing
-- Hyphen for range
parseClassBody ('-':rest) [] _ _ =
  parseClassBody rest ['-'] False Nothing
parseClassBody ('-':rest) options@(o:_) _ _ =
  parseClassBody rest options True (Just o)
-- Regular character
parseClassBody (c:rest) options _ _ =
  parseClassBody rest (c : options) False Nothing

-- | Expand escape sequences inside character classes.
classEscapeToChars :: Char -> [Char]
classEscapeToChars 'd' = numericChars
classEscapeToChars 'D' = nonDigitChars
classEscapeToChars 'w' = alphanumericChars ++ "_"
classEscapeToChars 'W' = nonWordChars
classEscapeToChars 's' = [' ', '\t', '\n', '\r']
classEscapeToChars 'S' = filter (`notElem` [' ', '\t', '\n', '\r']) printableChars
classEscapeToChars c   = [c]  -- Literal escapes: ], -, \, etc.

-- | Parse a POSIX character class like [:alpha:].
-- Takes the characters after "[:".
-- Returns (list of chars, remaining after ":]").
parsePosixCharacterClass :: String -> ([Char], String)
parsePosixCharacterClass = go []
  where
    go _ [] = ([], [])
    go nameChars (':':']':rest) =
      let className = reverse nameChars
      in (posixClassToChars className, rest)
    go nameChars (c:rest) = go (c : nameChars) rest

-- | Convert POSIX character class name to list of characters.
posixClassToChars :: String -> [Char]
posixClassToChars "alpha"    = alphabeticChars
posixClassToChars "digit"    = numericChars
posixClassToChars "alphanum" = alphanumericChars
posixClassToChars "alnum"    = alphanumericChars
posixClassToChars "word"     = alphanumericChars ++ "_"
posixClassToChars "blank"    = blankChars
posixClassToChars "space"    = blankChars ++ ['\n', '\r']
posixClassToChars "upper"    = uppercaseChars
posixClassToChars "lower"    = lowercaseChars
posixClassToChars "ascii"    = asciiChars
posixClassToChars "cntrl"    = controlChars
posixClassToChars "graph"    = graphicChars
posixClassToChars "print"    = printableChars
posixClassToChars "punct"    = punctuationChars
posixClassToChars "xdigit"   = hexDigitChars
-- Fallback for unknown POSIX classes: use all ASCII characters to avoid empty classes.
posixClassToChars _          = asciiChars

-- | Parse a quantifier like {5} or {5+} or {5++} etc.
-- Takes the characters after the opening '{'.
-- Returns (Quantifier, remaining characters).
parseQuantifier :: String -> (Quantifier, String)
parseQuantifier = parseDigits []
  where
    parseDigits :: String -> String -> (Quantifier, String)
    parseDigits digits [] =
      let n = readInt (reverse digits)
      in (Exact n, [])
    parseDigits digits ('}':rest) =
      let n = readInt (reverse digits)
      in (Exact (if n >= 0 then n else 1), rest)
    parseDigits digits ('+':rest) =
      countPlus digits rest 1
    parseDigits digits (c:rest)
      | c >= '0' && c <= '9' = parseDigits (c : digits) rest
      | otherwise = (Exact 1, c:rest)

    countPlus :: String -> String -> Int -> (Quantifier, String)
    countPlus digits [] plusCount =
      let n = readInt (reverse digits)
      in (Normal (max 0 n) (plusCount + 1), [])
    countPlus digits ('+':rest) plusCount =
      countPlus digits rest (plusCount + 1)
    countPlus digits ('}':rest) plusCount =
      let n = readInt (reverse digits)
      in (Normal (max 0 n) (plusCount + 1), rest)
    countPlus digits (c:rest) plusCount
      | c >= '0' && c <= '9' =
        -- This is a range: {n1+...n2} or {+...n2}
        parseSecondNumber digits [c] rest plusCount
      | otherwise =
        let n = readInt (reverse digits)
        in (Normal (max 0 n) (plusCount + 1), c:rest)

    parseSecondNumber :: String -> String -> String -> Int -> (Quantifier, String)
    parseSecondNumber digits1 digits2 [] plusCount =
      let n1 = readInt (reverse digits1)
          n2 = readInt (reverse digits2)
      in (NormalRange (max 0 n1) (max 0 n2) (plusCount + 1), [])
    parseSecondNumber digits1 digits2 ('}':rest) plusCount =
      let n1 = readInt (reverse digits1)
          n2 = readInt (reverse digits2)
      in (NormalRange (max 0 n1) (max 0 n2) (plusCount + 1), rest)
    parseSecondNumber digits1 digits2 (c:rest) plusCount
      | c >= '0' && c <= '9' = parseSecondNumber digits1 (c : digits2) rest plusCount
      | otherwise =
        let n1 = readInt (reverse digits1)
            n2 = readInt (reverse digits2)
        in (NormalRange (max 0 n1) (max 0 n2) (plusCount + 1), c:rest)

    readInt :: String -> Int
    readInt [] = 0
    readInt s = case reads s :: [(Int, String)] of
      ((n, _):_) -> n
      _          -> 0

-- | Parse a group like (abc).
-- Takes the characters after the opening '('.
-- Returns (group content string, remaining characters).
parseGroup :: String -> (String, String)
parseGroup chars = parseGroupInternal chars 1

-- | Internal function to parse a group with nesting level.
parseGroupInternal :: String -> Int -> (String, String)
parseGroupInternal = go []
  where
    go :: String -> String -> Int -> (String, String)
    go groupChars [] _ = (reverse groupChars, [])
    go groupChars (')':rest) 1 = (reverse groupChars, rest)
    go groupChars ('(':rest) n = go ('(' : groupChars) rest (n + 1)
    go groupChars ('|':rest) 1 = go ('|' : groupChars) rest 1
    go groupChars (')':rest) n = go (')' : groupChars) rest (n - 1)
    go groupChars (c:rest)   n = go (c : groupChars) rest n

-- | Parse a named group like (?<name>...) - starts after the "<".
-- Returns (name, group content, remaining characters).
parseNamedGroup :: String -> (String, String, String)
parseNamedGroup = nameLoop []
  where
    nameLoop :: String -> String -> (String, String, String)
    nameLoop _         [] = ("", "", [])
    nameLoop nameChars ('>':rest) =
      let name = reverse nameChars
          (groupContent, remaining) = parseGroupInternal rest 1
      in (name, groupContent, remaining)
    nameLoop nameChars (c:rest) = nameLoop (c : nameChars) rest

-- | Parse a backreference name like \k<name> - starts after the "<".
-- Returns (name, remaining characters).
parseBackreferenceName :: String -> (String, String)
parseBackreferenceName = go []
  where
    go :: String -> String -> (String, String)
    go nameChars [] = (reverse nameChars, [])
    go nameChars ('>':rest) = (reverse nameChars, rest)
    go nameChars (c:rest)   = go (c : nameChars) rest

-- | Parse a Unicode property like {L} or {Script=Han}.
-- Takes the characters after "\p{".
-- Returns (property name, remaining characters).
parseUnicodeProperty :: String -> (String, String)
parseUnicodeProperty = go []
  where
    go :: String -> String -> (String, String)
    go _ [] = ("", [])
    go propChars ('}':rest) = (reverse propChars, rest)
    go propChars (c:rest)   = go (c : propChars) rest

-- | Convert a character range to a list of characters.
rangeToList :: Char -> Char -> [Char]
rangeToList start end = [start..end]
