-- | Pattern tokenizer — parses regex-like pattern strings into tokens.
-- Faithfully reimplements tokenizer.rkt from the Racket version.
module RandStr.Tokenizer
  ( tokenizePattern
  ) where

import RandStr.Types
import RandStr.Utils (removeDuplicatesPreservingOrder)
import RandStr.CharClasses (posixClassChars, wordCharString, numericChars,
                            nonWordChars, nonDigitChars, whitespaceChars,
                            nonWhitespaceChars)
import Data.Char (chr, isDigit, isAlpha)

-- | Tokenize a pattern string into a list of Tokens.
tokenizePattern :: String -> [Token]
tokenizePattern = reverse . go []
  where
    go acc [] = acc
    go acc (c:cs) = case c of
      -- Anchors: skip
      '^' -> go acc cs
      '$' -> go acc cs

      -- Character class
      '[' -> let (tok, rest) = parseCharacterClass cs
             in go (tok : acc) rest

      -- Group
      '(' -> case cs of
        -- Named group (?<name>...)
        '?':'<':rest ->
          let (name, afterName) = parseNameUntilGt rest
              (content, afterGroup) = parseGroupInternal afterName
              tok = Token TNamedGroup (ContentNamedGroup name content) Nothing
          in applyPostQuantifier tok afterGroup acc
        -- Regular group
        _ ->
          let (content, afterGroup) = parseGroupInternal cs
              tok = Token TGroup (ContentString content) Nothing
          in applyPostQuantifier tok afterGroup acc

      -- Quantifiers on previous token
      '{' -> case acc of
        [] -> go acc cs  -- No preceding token, skip
        (prev:rest) ->
          let (q, afterQ) = parseQuantifier cs
              prev' = prev { tokenQuantifier = Just q }
          in go (prev' : rest) afterQ

      '*' -> case acc of
        [] -> go acc cs
        (prev:rest) -> go (prev { tokenQuantifier = Just QStar } : rest) cs

      '+' -> case acc of
        [] -> go acc cs
        (prev:rest) -> go (prev { tokenQuantifier = Just QPlus } : rest) cs

      '?' -> case acc of
        [] -> go acc cs
        (prev:rest) -> go (prev { tokenQuantifier = Just QOptional } : rest) cs

      -- Any printable character
      '.' -> go (Token TAny ContentNone Nothing : acc) cs

      -- Escape sequences
      '\\' -> case cs of
        [] -> go acc cs  -- Trailing backslash, ignore
        (e:erest) -> case e of
          'w' -> go (Token TWordChar ContentNone Nothing : acc) erest
          'W' -> go (Token TNonWordChar ContentNone Nothing : acc) erest
          's' -> go (Token TWhitespaceChar ContentNone Nothing : acc) erest
          'S' -> go (Token TNonWhitespaceChar ContentNone Nothing : acc) erest
          'd' -> go (Token TDigitChar ContentNone Nothing : acc) erest
          'D' -> go (Token TNonDigitChar ContentNone Nothing : acc) erest

          -- Unicode property \p{...}
          'p' -> case erest of
            '{':propRest ->
              let (propName, afterProp) = parseUnicodeProperty propRest
                  tok = Token TUnicodeProperty (ContentUnicode propName) Nothing
              in go (tok : acc) afterProp
            _ -> go (Token TLiteral (ContentChar 'p') Nothing : acc) erest

          -- Backreference \k<name>
          'k' -> case erest of
            '<':nameRest ->
              let (name, afterName) = parseBackreferenceName nameRest
                  tok = Token TBackreference (ContentString name) Nothing
              in go (tok : acc) afterName
            _ -> go (Token TLiteral (ContentChar 'k') Nothing : acc) erest

          -- Other escaped character → literal
          _ -> go (Token TLiteral (ContentChar e) Nothing : acc) erest

      -- Literal character
      _ -> go (Token TLiteral (ContentChar c) Nothing : acc) cs

    -- After parsing a group/class, check for a following quantifier
    applyPostQuantifier tok rest acc = case rest of
      '{':qs ->
        let (q, afterQ) = parseQuantifier qs
            tok' = tok { tokenQuantifier = Just q }
        in go (tok' : acc) afterQ
      '*':qs -> go (tok { tokenQuantifier = Just QStar } : acc) qs
      '+':qs -> go (tok { tokenQuantifier = Just QPlus } : acc) qs
      '?':qs -> go (tok { tokenQuantifier = Just QOptional } : acc) qs
      _      -> go (tok : acc) rest

-- | Parse a character class after the opening '['.
-- Returns (Token, remaining string after ']').
parseCharacterClass :: String -> (Token, String)
parseCharacterClass input =
  let (negated, rest0) = case input of
        '^':r -> (True, r)
        _     -> (False, input)
      (chars, rest1) = collectClassChars rest0
      deduped = removeDuplicatesPreservingOrder chars
      finalChars = if negated
                   then complement deduped
                   else deduped
      arr = charArrayFromList finalChars
  in (Token TCharClass (ContentChars arr) Nothing, rest1)

-- | Complement of a character set within printable ASCII (32-126).
complement :: [Char] -> [Char]
complement exclude =
  let isExcluded ch = ch `elem` exclude
  in filter (not . isExcluded) [chr 32 .. chr 126]

-- | Collect characters inside a character class until unescaped ']'.
collectClassChars :: String -> ([Char], String)
collectClassChars = go []
  where
    go acc [] = (reverse acc, [])
    go acc (']':rest) = (reverse acc, rest)

    -- Nested POSIX class [[:name:]]
    go acc ('[':':':rest) =
      let (name, afterPosix) = span isAlpha rest
      in case afterPosix of
        ':':']':r -> go (reverse (posixClassChars name) ++ acc) r
        _         -> go ('[' : acc) (':':rest)

    -- Escape sequences inside class
    go acc ('\\':e:rest) = case e of
      'd' -> go (reverse numericChars ++ acc) rest
      'D' -> go (reverse nonDigitChars ++ acc) rest
      'w' -> go (reverse wordCharString ++ acc) rest
      'W' -> go (reverse nonWordChars ++ acc) rest
      's' -> go (reverse whitespaceChars ++ acc) rest
      'S' -> go (reverse nonWhitespaceChars ++ acc) rest
      ']' -> go (']' : acc) rest
      '-' -> go ('-' : acc) rest
      '\\' -> go ('\\' : acc) rest
      _   -> go (e : acc) rest

    -- Range: a-z
    go acc (c:'-':e:rest)
      | e /= ']' = go (reverse [c..e] ++ acc) rest

    -- Regular character
    go acc (c:rest) = go (c : acc) rest

-- | Parse a quantifier after '{'. Returns (Quantifier, remaining string after '}').
parseQuantifier :: String -> (Quantifier, String)
parseQuantifier input =
  let (content, rest) = span (/= '}') input
      afterBrace = case rest of
        '}':r -> r
        _     -> rest
  in (parseQuantifierContent content, afterBrace)

-- | Parse the content inside {}.
parseQuantifierContent :: String -> Quantifier
parseQuantifierContent s = case s of
  -- {++n} → QNormalRange 0 n 3
  '+':'+':numStr | all isDigit numStr && not (null numStr) ->
    QNormalRange 0 (read numStr) 3

  -- {+n} → QNormalRange 0 n 2
  '+':numStr | all isDigit numStr && not (null numStr) ->
    QNormalRange 0 (read numStr) 2

  _ -> case break (== '+') s of
    -- {n} — no plus sign, exact
    (numStr, "") | all isDigit numStr && not (null numStr) ->
      QExact (read numStr)

    -- {n1+...}
    (n1Str, '+':rest) | all isDigit n1Str && not (null n1Str) ->
      let n1 = read n1Str
      in case rest of
        -- {n++} → QNormal n 3
        '+':r | null r      -> QNormal n1 3
        -- {n+n2} → QNormalRange n1 n2 2
        _     | all isDigit rest && not (null rest) ->
                QNormalRange n1 (read rest) 2
        -- {n+} → QNormal n 2
              | null rest   -> QNormal n1 2
        -- {n++n2} — check for ++
              | otherwise -> case rest of
                  '+':r2 | all isDigit r2 && not (null r2) ->
                    QNormalRange n1 (read r2) 3
                  _ -> QExact 1  -- Fallback

    _ -> QExact 1  -- Fallback for unparseable

-- | Parse group content after '(' or after named group '(?<name>'.
-- Tracks nesting depth, returns (content, remaining string after ')').
parseGroupInternal :: String -> (String, String)
parseGroupInternal = go (0 :: Int) []
  where
    go _ acc [] = (reverse acc, [])
    go 0 acc (')':rest) = (reverse acc, rest)
    go depth acc (c:rest) = case c of
      '(' -> go (depth + 1) (c : acc) rest
      ')' -> go (depth - 1) (c : acc) rest
      '\\' -> case rest of
        (e:er) -> go depth (e : c : acc) er
        []     -> go depth (c : acc) []
      _   -> go depth (c : acc) rest

-- | Parse a named group name after '(?<', reading until '>'.
parseNameUntilGt :: String -> (String, String)
parseNameUntilGt = go []
  where
    go acc [] = (reverse acc, [])
    go acc ('>':rest) = (reverse acc, rest)
    go acc (c:rest) = go (c : acc) rest

-- | Parse a backreference name after '\k<', reading until '>'.
parseBackreferenceName :: String -> (String, String)
parseBackreferenceName = parseNameUntilGt

-- | Parse a Unicode property name after '\p{', reading until '}'.
parseUnicodeProperty :: String -> (String, String)
parseUnicodeProperty = go []
  where
    go acc [] = (reverse acc, [])
    go acc ('}':rest) = (reverse acc, rest)
    go acc (c:rest) = go (c : acc) rest
