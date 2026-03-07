-- | CLI interface for the randstr tool.
module Main (main) where

import Randstr.Lib (randstrWith, randstrNWith, Config(..), defaultConfig)
import System.Environment (getArgs, lookupEnv)
import System.Random (newStdGen)
import System.Exit (exitFailure)
import Data.Char (toLower)
import Data.List (isPrefixOf)

-- | Parse command-line arguments.
data CLIOptions = CLIOptions
  { optCount     :: Int
  , optMaxRepeat :: Maybe Int
  , optSecure    :: Bool
  , optPattern   :: String
  } deriving (Show)

defaultOptions :: CLIOptions
defaultOptions = CLIOptions
  { optCount     = 1
  , optMaxRepeat = Nothing
  , optSecure    = False
  , optPattern   = ""
  }

helpText :: String
helpText = unlines
  [ "Usage: randstr [options] <pattern>"
  , "Generate random strings based on regex-like patterns."
  , ""
  , "Options:"
  , "  -n, --count N       Generate N strings (default: 1)"
  , "  -m, --max-repeat N  Maximum repetition for * and + (default: env RANDSTR_MAX_REPEAT or 5)"
  , "  -s, --secure        Enable secure random mode (not yet implemented; will error)"
  , "  -h, --help          Show this help message"
  ]

-- | Parse arguments into CLIOptions.
parseArgs :: [String] -> Either String CLIOptions
parseArgs = go defaultOptions
  where
    go opts [] = Right opts
    go _    ("-h":_) = Left helpText
    go _    ("--help":_) = Left helpText
    go opts ("-n":n:rest) = case reads n :: [(Int, String)] of
      ((v, ""):_) | v >= 1 -> go (opts { optCount = v }) rest
      _ -> Left $ "Invalid count (must be >= 1): " ++ n
    go opts ("--count":n:rest) = case reads n :: [(Int, String)] of
      ((v, ""):_) | v >= 1 -> go (opts { optCount = v }) rest
      _ -> Left $ "Invalid count (must be >= 1): " ++ n
    go opts ("-m":n:rest) = case reads n :: [(Int, String)] of
      ((v, ""):_) | v >= 0 -> go (opts { optMaxRepeat = Just v }) rest
      _ -> Left $ "Invalid max-repeat (must be >= 0): " ++ n
    go opts ("--max-repeat":n:rest) = case reads n :: [(Int, String)] of
      ((v, ""):_) | v >= 0 -> go (opts { optMaxRepeat = Just v }) rest
      _ -> Left $ "Invalid max-repeat (must be >= 0): " ++ n
    go opts ("-s":rest) = go (opts { optSecure = True }) rest
    go opts ("--secure":rest) = go (opts { optSecure = True }) rest
    go opts [pat] = Right (opts { optPattern = pat })
    go _ (unknown:_) = Left $ "Unknown option: " ++ unknown

-- | Read a positive integer from an environment variable.
envToPositiveInteger :: String -> IO (Maybe Int)
envToPositiveInteger name = do
  val <- lookupEnv name
  return $ case val of
    Nothing -> Nothing
    Just "" -> Nothing
    Just v  -> case reads v :: [(Int, String)] of
      ((n, ""):_) | n > 0 -> Just n
      _ -> Nothing

-- | Read a boolean from an environment variable.
envToBoolean :: String -> IO Bool
envToBoolean name = do
  val <- lookupEnv name
  return $ case val of
    Nothing -> False
    Just "" -> False
    Just v  -> map toLower v `elem` ["1", "true", "yes", "on"]

main :: IO ()
main = do
  args <- getArgs
  case parseArgs args of
    Left msg -> do
      putStr msg
      if "Usage:" `isPrefixOf` msg then return () else exitFailure
    Right opts
      | null (optPattern opts) -> do
          putStr helpText
          exitFailure
      | otherwise -> do
          -- Read environment variables
          envMaxRepeat <- envToPositiveInteger "RANDSTR_MAX_REPEAT"
          envSecure <- envToBoolean "RANDSTR_SECURE"

          -- Determine final configuration
          let maxRepeat = case optMaxRepeat opts of
                Just mr -> mr
                Nothing -> case envMaxRepeat of
                  Just mr -> mr
                  Nothing -> 5
              secure = optSecure opts || envSecure

          gen <- newStdGen
          let cfg = (defaultConfig gen)
                { cfgMaxRepeat = maxRepeat
                , cfgSecureRandom = secure
                }
              pat = optPattern opts
              count = optCount opts

          if count == 1
            then putStrLn (randstrWith cfg pat)
            else mapM_ putStrLn (randstrNWith cfg pat count)
