-- | Generator for producing random strings from tokens.
module Randstr.Generator
  ( generateFromTokens
  ) where

import Randstr.Types
import Randstr.Config
import Randstr.Tokenizer (tokenizePattern)
import qualified Randstr.CharClasses as CC
import qualified Data.Map.Strict as Map
import qualified Data.Vector as V

-- | Environment for named groups (maps name to generated string).
type Env = Map.Map String String

-- | Generate a random string from a list of tokens.
generateFromTokens :: [Token] -> Config -> (String, Config)
generateFromTokens tokens cfg =
  let (result, _, cfg') = generateWithEnv tokens Map.empty cfg
  in (result, cfg')

-- | Internal: Generate string from tokens with an environment for named groups.
generateWithEnv :: [Token] -> Env -> Config -> (String, Env, Config)
generateWithEnv [] env cfg = ("", env, cfg)
generateWithEnv (tok:rest) env cfg =
  let (tokStr, env', cfg') = generateToken tok env cfg
      (restStr, env'', cfg'') = generateWithEnv rest env' cfg'
  in (tokStr ++ restStr, env'', cfg'')

-- | Generate string for a single token.
generateToken :: Token -> Env -> Config -> (String, Env, Config)
generateToken tok env cfg = case (tokenType tok, tokenContent tok) of
  (Literal, CharContent c) ->
    let (chars, cfg') = applyQuantifierChar c (tokenQuantifier tok) cfg
    in (chars, env, cfg')

  (CharClass, CharsContent options) ->
    let vec = V.fromList options
        charFunc cfg0 = CC.vectorRandomRef vec cfg0
        (chars, cfg') = applyQuantifierFunc charFunc (tokenQuantifier tok) cfg
    in (chars, env, cfg')

  (WordChar, _) ->
    let (chars, cfg') = applyQuantifierFunc CC.randomWordChar (tokenQuantifier tok) cfg
    in (chars, env, cfg')

  (NonWordChar, _) ->
    let (chars, cfg') = applyQuantifierFunc CC.randomNonWordChar (tokenQuantifier tok) cfg
    in (chars, env, cfg')

  (DigitChar, _) ->
    let (chars, cfg') = applyQuantifierFunc CC.randomDigitChar (tokenQuantifier tok) cfg
    in (chars, env, cfg')

  (NonDigitChar, _) ->
    let (chars, cfg') = applyQuantifierFunc CC.randomNonDigitChar (tokenQuantifier tok) cfg
    in (chars, env, cfg')

  (WhitespaceChar, _) ->
    let (chars, cfg') = applyQuantifierFunc CC.randomWhitespaceChar (tokenQuantifier tok) cfg
    in (chars, env, cfg')

  (NonWhitespaceChar, _) ->
    let (chars, cfg') = applyQuantifierFunc CC.randomNonWhitespaceChar (tokenQuantifier tok) cfg
    in (chars, env, cfg')

  (AnyChar, _) ->
    let (chars, cfg') = applyQuantifierFunc CC.randomCharacter (tokenQuantifier tok) cfg
    in (chars, env, cfg')

  (UnicodeProperty, StringContent property) ->
    let propertyChars = CC.unicodePropertyChars property
        charFunc cfg0 = if null propertyChars
                        then ('?', cfg0)
                        else CC.randomRef propertyChars cfg0
        (chars, cfg') = applyQuantifierFunc charFunc (tokenQuantifier tok) cfg
    in (chars, env, cfg')

  (Group, StringContent groupPattern) ->
    let alternatives = splitTopLevelAlternatives groupPattern
        (str, cfg') = if length alternatives == 1
          then applyQuantifierString
                 (\cfg0 -> let subTokens = tokenizePattern groupPattern
                               (s, cfg1) = generateFromTokensWithEnv subTokens env cfg0
                           in (s, cfg1))
                 (tokenQuantifier tok) cfg
          else applyQuantifierString
                 (\cfg0 -> let (idx, cfg1) = configRandom (length alternatives) cfg0
                               selected = alternatives !! idx
                               subTokens = tokenizePattern selected
                               (s, cfg2) = generateFromTokensWithEnv subTokens env cfg1
                           in (s, cfg2))
                 (tokenQuantifier tok) cfg
    in (str, env, cfg')

  (NamedGroup, NamedGroupContent name groupPattern) ->
    let subTokens = tokenizePattern groupPattern
        (subString, cfg') = generateFromTokensWithEnv subTokens env cfg
        newEnv = Map.insert name subString env
        (str, cfg'') = applyQuantifierString (\cfg0 -> (subString, cfg0)) (tokenQuantifier tok) cfg'
    in (str, newEnv, cfg'')

  (Backreference, StringContent name) ->
    let storedString = Map.findWithDefault "" name env
        (str, cfg') = applyQuantifierString (\cfg0 -> (storedString, cfg0)) (tokenQuantifier tok) cfg
    in (str, env, cfg')

  -- Fallback for unexpected combinations
  _ -> ("", env, cfg)

-- | Helper: generate from tokens with environment (returns string and config, discarding env).
generateFromTokensWithEnv :: [Token] -> Env -> Config -> (String, Config)
generateFromTokensWithEnv tokens env cfg =
  let (result, _, cfg') = generateWithEnv tokens env cfg
  in (result, cfg')

-- | Split group content into top-level alternatives.
-- Only splits on an unescaped '|' when not inside (...) or [...].
splitTopLevelAlternatives :: String -> [String]
splitTopLevelAlternatives s = reverse $ map reverse $ go s [] [] 0 0 False
  where
    go :: String -> String -> [String] -> Int -> Int -> Bool -> [String]
    go [] current parts _ _ _ = current : parts
    go (c:rest) current parts parenDepth bracketDepth escaped
      | escaped = go rest (c : current) parts parenDepth bracketDepth False
      | c == '\\' = go rest (c : current) parts parenDepth bracketDepth True
      | c == '[' = go rest (c : current) parts parenDepth (bracketDepth + 1) False
      | c == ']' = go rest (c : current) parts parenDepth (max 0 (bracketDepth - 1)) False
      | c == '(' = go rest (c : current) parts (parenDepth + 1) bracketDepth False
      | c == ')' = go rest (c : current) parts (max 0 (parenDepth - 1)) bracketDepth False
      | c == '|' && parenDepth == 0 && bracketDepth == 0 =
          go rest [] (current : parts) 0 0 False
      | otherwise = go rest (c : current) parts parenDepth bracketDepth False

-- | Apply quantifier to a single character.
applyQuantifierChar :: Char -> Quantifier -> Config -> (String, Config)
applyQuantifierChar c quant cfg = case quant of
  NoQuantifier -> ([c], cfg)
  Exact n -> (replicate n c, cfg)
  Star ->
    let maxR = cfgMaxRepeat cfg
        (count, cfg') = configRandom (maxR + 1) cfg
    in (replicate count c, cfg')
  Plus ->
    let maxR = cfgMaxRepeat cfg
        (count, cfg') = configRandom (max 1 maxR) cfg
    in (replicate (count + 1) c, cfg')
  Optional ->
    let (coin, cfg') = configRandom 2 cfg
    in (if coin == 0 then "" else [c], cfg')
  Normal mean order ->
    let (count, cfg') = normalSample mean order cfg
    in (replicate count c, cfg')
  NormalRange minVal maxVal order ->
    let (count, cfg') = normalRangeSample minVal maxVal order cfg
    in (replicate count c, cfg')

-- | Apply quantifier using a character-generating function.
applyQuantifierFunc :: (Config -> (Char, Config)) -> Quantifier -> Config -> (String, Config)
applyQuantifierFunc f quant cfg = case quant of
  NoQuantifier ->
    let (c, cfg') = f cfg
    in ([c], cfg')
  Exact n -> repeatFunc f n cfg
  Star ->
    let maxR = cfgMaxRepeat cfg
        (count, cfg') = configRandom (maxR + 1) cfg
    in repeatFunc f count cfg'
  Plus ->
    let maxR = cfgMaxRepeat cfg
        (count, cfg') = configRandom (max 1 maxR) cfg
    in repeatFunc f (count + 1) cfg'
  Optional ->
    let (coin, cfg') = configRandom 2 cfg
    in if coin == 0
       then ("", cfg')
       else let (c, cfg'') = f cfg'
            in ([c], cfg'')
  Normal mean order ->
    let (count, cfg') = normalSample mean order cfg
    in repeatFunc f count cfg'
  NormalRange minVal maxVal order ->
    let (count, cfg') = normalRangeSample minVal maxVal order cfg
    in repeatFunc f count cfg'

-- | Apply quantifier using a string-generating function.
applyQuantifierString :: (Config -> (String, Config)) -> Quantifier -> Config -> (String, Config)
applyQuantifierString f quant cfg = case quant of
  NoQuantifier -> f cfg
  Exact n -> repeatStringFunc f n cfg
  Star ->
    let maxR = cfgMaxRepeat cfg
        (count, cfg') = configRandom (maxR + 1) cfg
    in repeatStringFunc f count cfg'
  Plus ->
    let maxR = cfgMaxRepeat cfg
        (count, cfg') = configRandom (max 1 maxR) cfg
    in repeatStringFunc f (count + 1) cfg'
  Optional ->
    let (coin, cfg') = configRandom 2 cfg
    in if coin == 0
       then ("", cfg')
       else f cfg'
  Normal mean order ->
    let (count, cfg') = normalSample mean order cfg
    in repeatStringFunc f count cfg'
  NormalRange minVal maxVal order ->
    let (count, cfg') = normalRangeSample minVal maxVal order cfg
    in repeatStringFunc f count cfg'

-- | Repeat a character-generating function n times.
repeatFunc :: (Config -> (Char, Config)) -> Int -> Config -> (String, Config)
repeatFunc _ 0 cfg = ("", cfg)
repeatFunc f n cfg =
  let (c, cfg') = f cfg
      (rest, cfg'') = repeatFunc f (n - 1) cfg'
  in (c : rest, cfg'')

-- | Repeat a string-generating function n times.
repeatStringFunc :: (Config -> (String, Config)) -> Int -> Config -> (String, Config)
repeatStringFunc _ 0 cfg = ("", cfg)
repeatStringFunc f n cfg =
  let (s, cfg') = f cfg
      (rest, cfg'') = repeatStringFunc f (n - 1) cfg'
  in (s ++ rest, cfg'')

-- | Generate a normal distribution sample.
-- Uses Central Limit Theorem (average of uniform samples).
-- The scaling factor sqrt(3/order) normalizes the variance of the averaged
-- uniform samples: a single Uniform(0,1) has variance 1/12, so the averaged
-- sum has variance 1/(12*order). Multiplying by sqrt(3/order) scales the
-- deviation to have a standard deviation proportional to 1/sqrt(4*order).
normalSample :: Int -> Int -> Config -> (Int, Config)
normalSample mean order cfg =
  let (samples, cfg') = getNSamples order cfg
      avg = sum samples / fromIntegral order
      deviation = avg - 0.5
      scaledDeviation = deviation * sqrt (3.0 / fromIntegral order)
      result = round (fromIntegral mean * (1.0 + scaledDeviation))
  in (max 1 result, cfg')

-- | Generate a normal distribution sample within a range [minVal, maxVal].
normalRangeSample :: Int -> Int -> Int -> Config -> (Int, Config)
normalRangeSample minVal maxVal order cfg =
  let (samples, cfg') = getNSamples order cfg
      avg = sum samples / fromIntegral order
      range' = fromIntegral (maxVal - minVal) :: Double
      result = round (fromIntegral minVal + avg * range')
  in (max minVal (min maxVal result), cfg')

-- | Get n random samples in [0, 1).
getNSamples :: Int -> Config -> ([Double], Config)
getNSamples 0 cfg = ([], cfg)
getNSamples n cfg =
  let (sample, cfg') = configRandomReal cfg
      (rest, cfg'') = getNSamples (n - 1) cfg'
  in (sample : rest, cfg'')
