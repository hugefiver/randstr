-- | String generator from parsed tokens.
-- Faithfully reimplements generator.rkt from the Racket version.
module RandStr.Generator
  ( generateFromTokens
  ) where

import RandStr.Types
import RandStr.Config
import RandStr.CharClasses
import RandStr.Tokenizer (tokenizePattern)
import qualified Data.Map.Strict as Map

-- | Environment mapping named group names to their generated values.
type Env = Map.Map String String

-- | Generate a random string from a list of tokens.
generateFromTokens :: Config -> [Token] -> IO String
generateFromTokens cfg tokens = do
  (result, _env) <- generateWithEnv cfg tokens Map.empty
  return result

-- | Generate with an environment for named groups/backreferences.
generateWithEnv :: Config -> [Token] -> Env -> IO (String, Env)
generateWithEnv _cfg [] env = return ("", env)
generateWithEnv cfg (tok:rest) env = do
  (s, env') <- generateToken cfg tok env
  (s', env'') <- generateWithEnv cfg rest env'
  return (s ++ s', env'')

-- | Generate the string for a single token.
generateToken :: Config -> Token -> Env -> IO (String, Env)
generateToken cfg tok env = case tokenType tok of
  TLiteral -> case tokenContent tok of
    ContentChar c -> do
      s <- applyQuantifier cfg (tokenQuantifier tok) (return [c]) (\n -> return [replicate n c])
      return (s, env)
    _ -> return ("", env)

  TCharClass -> case tokenContent tok of
    ContentChars arr -> do
      s <- applyQuantifier cfg (tokenQuantifier tok)
             (fmap (:[]) (arrayRandomRef cfg arr))
             (\n -> sequence $ replicate n (fmap (:[]) (arrayRandomRef cfg arr)))
      return (s, env)
    _ -> return ("", env)

  TWordChar -> do
    s <- applyQuantifier cfg (tokenQuantifier tok)
           (fmap (:[]) (randomWordChar cfg))
           (\n -> sequence $ replicate n (fmap (:[]) (randomWordChar cfg)))
    return (s, env)

  TNonWordChar -> do
    s <- applyQuantifier cfg (tokenQuantifier tok)
           (fmap (:[]) (randomNonWordChar cfg))
           (\n -> sequence $ replicate n (fmap (:[]) (randomNonWordChar cfg)))
    return (s, env)

  TWhitespaceChar -> do
    s <- applyQuantifier cfg (tokenQuantifier tok)
           (fmap (:[]) (randomWhitespaceChar cfg))
           (\n -> sequence $ replicate n (fmap (:[]) (randomWhitespaceChar cfg)))
    return (s, env)

  TNonWhitespaceChar -> do
    s <- applyQuantifier cfg (tokenQuantifier tok)
           (fmap (:[]) (randomNonWhitespaceChar cfg))
           (\n -> sequence $ replicate n (fmap (:[]) (randomNonWhitespaceChar cfg)))
    return (s, env)

  TDigitChar -> do
    s <- applyQuantifier cfg (tokenQuantifier tok)
           (fmap (:[]) (randomDigitChar cfg))
           (\n -> sequence $ replicate n (fmap (:[]) (randomDigitChar cfg)))
    return (s, env)

  TNonDigitChar -> do
    s <- applyQuantifier cfg (tokenQuantifier tok)
           (fmap (:[]) (randomNonDigitChar cfg))
           (\n -> sequence $ replicate n (fmap (:[]) (randomNonDigitChar cfg)))
    return (s, env)

  TAny -> do
    s <- applyQuantifier cfg (tokenQuantifier tok)
           (fmap (:[]) (randomCharacter cfg))
           (\n -> sequence $ replicate n (fmap (:[]) (randomCharacter cfg)))
    return (s, env)

  TGroup -> case tokenContent tok of
    ContentString pattern -> do
      s <- applyQuantifier cfg (tokenQuantifier tok)
             (generateGroup cfg pattern env)
             (\n -> sequence $ replicate n (generateGroup cfg pattern env))
      return (s, env)
    _ -> return ("", env)

  TNamedGroup -> case tokenContent tok of
    ContentNamedGroup name pattern -> do
      s <- applyQuantifier cfg (tokenQuantifier tok)
             (generateGroup cfg pattern env)
             (\n -> sequence $ replicate n (generateGroup cfg pattern env))
      let env' = Map.insert name s env
      return (s, env')
    _ -> return ("", env)

  TBackreference -> case tokenContent tok of
    ContentString name ->
      let val = Map.findWithDefault "" name env
      in return (val, env)
    _ -> return ("", env)

  TUnicodeProperty -> case tokenContent tok of
    ContentUnicode propName -> do
      let count = unicodePropertyCharCount propName
      if count == 0
        then return ("", env)
        else do
          s <- applyQuantifier cfg (tokenQuantifier tok)
                 (do idx <- randstrRandom cfg count
                     return [unicodePropertyCharAtIndex propName idx])
                 (\n -> sequence $ replicate n
                   (do idx <- randstrRandom cfg count
                       return [unicodePropertyCharAtIndex propName idx]))
          return (s, env)
    _ -> return ("", env)

-- | Generate a string for a group pattern, handling alternation with '|'.
generateGroup :: Config -> String -> Env -> IO String
generateGroup cfg pattern env = do
  let alts = splitTopLevelAlternatives pattern
  idx <- randstrRandom cfg (length alts)
  let chosen = alts !! idx
      tokens = tokenizePattern chosen
  (result, _) <- generateWithEnv cfg tokens env
  return result

-- | Apply a quantifier to produce a string.
-- Takes a single-generation action and a multi-generation action.
applyQuantifier :: Config -> Maybe Quantifier -> IO String -> (Int -> IO [String]) -> IO String
applyQuantifier cfg mq singleGen multiGen = case mq of
  Nothing -> singleGen

  Just QStar -> do
    n <- randstrRandom cfg (cfgMaxRepeat cfg + 1)  -- 0 to maxRepeat
    if n == 0 then return "" else fmap concat (multiGen n)

  Just QPlus -> do
    n <- randstrRandomRange cfg 1 (cfgMaxRepeat cfg)  -- 1 to maxRepeat
    fmap concat (multiGen n)

  Just QOptional -> do
    coin <- randstrRandom cfg 2
    if coin == 0 then return "" else singleGen

  Just (QExact n) ->
    fmap concat (multiGen n)

  Just (QNormal mean order) -> do
    n <- normalSample cfg mean order
    fmap concat (multiGen n)

  Just (QNormalRange rmin rmax order) -> do
    n <- normalRangeSample cfg rmin rmax order
    fmap concat (multiGen n)

-- | Normal distribution sample around a mean.
-- Averages 'order' uniform samples, computes scaled deviation.
-- Result = round(mean * (1 + scaled_deviation)), clamped to min 1.
normalSample :: Config -> Int -> Int -> IO Int
normalSample cfg mean order = do
  samples <- sequence $ replicate order (randstrRandomReal cfg)
  let avg = sum samples / fromIntegral order
      deviation = avg - 0.5
      scaleFactor = sqrt (3.0 / fromIntegral order)
      scaledDeviation = deviation * scaleFactor
      result = round (fromIntegral mean * (1.0 + scaledDeviation) :: Double)
  return (max 1 result)

-- | Normal range sample: maps uniform average to [rmin, rmin+rmax].
-- 'rmax' here is the offset from rmin (matching Racket's {n1+n2} semantics).
normalRangeSample :: Config -> Int -> Int -> Int -> IO Int
normalRangeSample cfg rmin rmax order = do
  samples <- sequence $ replicate order (randstrRandomReal cfg)
  let avg = sum samples / fromIntegral order
      lo = fromIntegral rmin :: Double
      hi = fromIntegral (rmin + rmax) :: Double
      result = round (lo + avg * (hi - lo))
  return (max rmin (min (rmin + rmax) result))

-- | Split a pattern string on top-level '|' characters.
-- Respects nesting of '(' ')' and '[' ']', and backslash escaping.
splitTopLevelAlternatives :: String -> [String]
splitTopLevelAlternatives = go 0 0 [] []
  where
    go :: Int -> Int -> String -> [String] -> String -> [String]
    go _ _ cur alts [] = reverse (reverse cur : alts)
    go pDepth bDepth cur alts (c:rest) = case c of
      '|' | pDepth == 0 && bDepth == 0 ->
        go 0 0 [] (reverse cur : alts) rest
      '(' -> go (pDepth + 1) bDepth (c : cur) alts rest
      ')' -> go (max 0 (pDepth - 1)) bDepth (c : cur) alts rest
      '[' -> go pDepth (bDepth + 1) (c : cur) alts rest
      ']' -> go pDepth (max 0 (bDepth - 1)) (c : cur) alts rest
      '\\' -> case rest of
        (e:er) -> go pDepth bDepth (e : c : cur) alts er
        []     -> go pDepth bDepth (c : cur) alts []
      _   -> go pDepth bDepth (c : cur) alts rest

