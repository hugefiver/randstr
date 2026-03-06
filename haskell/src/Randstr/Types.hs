-- | Core types for the randstr library.
module Randstr.Types
  ( TokenType(..)
  , Quantifier(..)
  , Token(..)
  , TokenContent
  , TokenContentData(..)
  ) where

-- | Token types representing different pattern elements.
data TokenType
  = Literal           -- ^ Plain character
  | CharClass         -- ^ Character class [abc] or [a-z]
  | WordChar          -- ^ \w - alphanumeric + underscore
  | NonWordChar       -- ^ \W - non-word characters
  | DigitChar         -- ^ \d - digits
  | NonDigitChar      -- ^ \D - non-digits
  | WhitespaceChar    -- ^ \s - whitespace
  | NonWhitespaceChar -- ^ \S - non-whitespace
  | AnyChar           -- ^ . - any printable character
  | Group             -- ^ (...) - group with optional alternation
  | NamedGroup        -- ^ (?<name>...) - named capture group
  | Backreference     -- ^ \k<name> - backreference
  | UnicodeProperty   -- ^ \p{...} - Unicode property
  deriving (Show, Eq)

-- | Quantifier types.
data Quantifier
  = NoQuantifier                         -- ^ No quantifier
  | Star                                 -- ^ * (0 or more)
  | Plus                                 -- ^ + (1 or more)
  | Optional                             -- ^ ? (0 or 1)
  | Exact Int                            -- ^ {n} (exactly n)
  | Normal Int Int                       -- ^ {n+}, {n++} etc. (normal distribution, mean n, order)
  | NormalRange Int Int Int              -- ^ {n1+n2}, {n1++n2} etc. (range normal distribution)
  deriving (Show, Eq)

-- | A token represents a parsed pattern element.
data Token = Token
  { tokenType      :: TokenType
  , tokenContent   :: TokenContent
  , tokenQuantifier :: Quantifier
  } deriving (Show, Eq)

-- | Content associated with a token.
-- Using a sum type to represent different content types.
type TokenContent = TokenContentData

data TokenContentData
  = CharContent Char              -- ^ Single character (for Literal)
  | CharsContent [Char]           -- ^ List of characters (for CharClass)
  | StringContent String          -- ^ String content (for Group, UnicodeProperty, Backreference)
  | NamedGroupContent String String -- ^ Name and pattern (for NamedGroup)
  | NoContent                     -- ^ No content (for WordChar, DigitChar, etc.)
  deriving (Show, Eq)
