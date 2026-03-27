-- | CLI entry point for randstr-hs.
-- Reimplements cli/main.rkt from the Racket version.
-- Uses manual argument parsing (no external dependencies).
module Main where

import RandStr (randstrN, Config(..), defaultConfig)
import System.Environment (getArgs, lookupEnv)
import Data.Char (toLower)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr, hSetEncoding, stdout, utf8)

-- | CLI options.
data Options = Options
  { optCount     :: Int      -- ^ Number of strings to generate (-n)
  , optMaxRepeat :: Int      -- ^ Max repeat for * and + (-m)
  , optSecure    :: Bool     -- ^ Use secure random (-s)
  , optPattern   :: String   -- ^ The pattern argument
  } deriving (Show)

main :: IO ()
main = do
  hSetEncoding stdout utf8
  args <- getArgs

  -- Read environment variables (lower priority than CLI flags)
  envMaxRepeat <- lookupEnv "RANDSTR_MAX_REPEAT"
  envSecure    <- lookupEnv "RANDSTR_SECURE"

  let envMaxRepeatVal = envMaxRepeat >>= parsePositiveInt
      envSecureVal    = maybe False parseBoolEnv envSecure

  -- Default values from env or config defaults
  let defCount     = 1
      defMaxRepeat = maybe (cfgMaxRepeat defaultConfig) id envMaxRepeatVal
      defSecure    = envSecureVal

  case parseArgs args defCount defMaxRepeat defSecure of
    Left err -> do
      hPutStrLn stderr $ "Error: " ++ err
      hPutStrLn stderr ""
      hPutStrLn stderr usage
      exitFailure
    Right opts -> do
      -- Validate
      if optCount opts < 1
        then do
          hPutStrLn stderr "Error: count must be at least 1"
          exitFailure
        else if optMaxRepeat opts < 1
        then do
          hPutStrLn stderr "Error: max-repeat must be at least 1"
          exitFailure
        else do
          let cfg = defaultConfig
                { cfgMaxRepeat    = optMaxRepeat opts
                , cfgSecureRandom = optSecure opts
                }
          results <- randstrN cfg (optPattern opts) (optCount opts)
          mapM_ putStrLn results

-- | Parse command-line arguments manually.
parseArgs :: [String] -> Int -> Int -> Bool -> Either String Options
parseArgs args defCount defMaxRepeat defSecure =
  go args defCount defMaxRepeat defSecure Nothing
  where
    go [] count maxRep sec (Just pat) = Right (Options count maxRep sec pat)
    go [] _     _      _   Nothing    = Left "missing required PATTERN argument"

    go ("--help":_) _ _ _ _ = Left "help requested"
    go ("-h":_) _ _ _ _     = Left "help requested"

    go ("-n":val:rest) _ maxRep sec pat = case parsePositiveInt val of
      Just n  -> go rest n maxRep sec pat
      Nothing -> Left $ "invalid count: " ++ val

    go ("--count":val:rest) _ maxRep sec pat = case parsePositiveInt val of
      Just n  -> go rest n maxRep sec pat
      Nothing -> Left $ "invalid count: " ++ val

    go ("-m":val:rest) count _ sec pat = case parsePositiveInt val of
      Just m  -> go rest count m sec pat
      Nothing -> Left $ "invalid max-repeat: " ++ val

    go ("--max-repeat":val:rest) count _ sec pat = case parsePositiveInt val of
      Just m  -> go rest count m sec pat
      Nothing -> Left $ "invalid max-repeat: " ++ val

    go ("-s":rest) count maxRep _ pat = go rest count maxRep True pat
    go ("--secure":rest) count maxRep _ pat = go rest count maxRep True pat

    -- Handle -nN (combined flag and value)
    go (('-':'n':val):rest) _ maxRep sec pat
      | not (null val) = case parsePositiveInt val of
          Just n  -> go rest n maxRep sec pat
          Nothing -> Left $ "invalid count: " ++ val

    go (('-':'m':val):rest) count _ sec pat
      | not (null val) = case parsePositiveInt val of
          Just m  -> go rest count m sec pat
          Nothing -> Left $ "invalid max-repeat: " ++ val

    -- Positional argument (pattern) — any arg not starting with '-'
    go (arg:rest) count maxRep sec Nothing = case arg of
      ('-':_) -> Left $ "unexpected argument: " ++ arg
      _       -> go rest count maxRep sec (Just arg)

    go (arg:_) _ _ _ _ = Left $ "unexpected argument: " ++ arg

-- | Usage text.
usage :: String
usage = unlines
  [ "Usage: randstr-hs [OPTIONS] PATTERN"
  , ""
  , "Generate random strings from regex-like patterns."
  , ""
  , "Options:"
  , "  -n, --count N        Number of strings to generate (default: 1)"
  , "  -m, --max-repeat N   Maximum repeat count for * and + (default: 5)"
  , "  -s, --secure         Use cryptographically secure random"
  , "  -h, --help           Show this help message"
  , ""
  , "Environment variables:"
  , "  RANDSTR_MAX_REPEAT   Default max repeat (positive integer)"
  , "  RANDSTR_SECURE       Enable secure random (1/true/yes/on)"
  ]

-- | Parse a string as a positive integer.
parsePositiveInt :: String -> Maybe Int
parsePositiveInt s = case reads s of
  [(n, "")] | n > 0 -> Just n
  _                  -> Nothing

-- | Parse a boolean environment variable value.
-- Accepts: 1, true, yes, on (case-insensitive) as True.
parseBoolEnv :: String -> Bool
parseBoolEnv s = map toLower s `elem` ["1", "true", "yes", "on"]
