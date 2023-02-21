{-# LANGUAGE OverloadedRecordDot #-}
-- {-# OPTIONS -fplugin=MonadicBang #-}

module Main where
import Data.Foldable (for_)
import System.IO (stdin, hReady, hGetEcho, hSetEcho, hGetBuffering, hSetBuffering, BufferMode (NoBuffering), hFlush, stdout, )
import Control.Exception.Base (bracket)
import Data.List (isPrefixOf, drop, elemIndex)
-- import Data.Maybe (fromMaybe, mapMaybe)
-- import System.Timeout (timeout)

data Options = Options {
  items :: [String]
  , defaultChoice :: Int
}

options :: Options
options = Options
  {
    items = ["Option one", "Option two", "Option three"]
    , defaultChoice = 2
  }

getKey :: IO [Char]
getKey = reverse <$> getKey' ""
  where getKey' chars = do
          char <- getChar
          more <- hReady stdin
          (if more then getKey' else return) (char:chars)

moveUp :: Int -> IO ()
moveUp whereTo = do
  putStr ("\ESC[" <> show (whereTo + 1) <> ";1H")
  putStr " "
  putStr ("\ESC[" <> show whereTo <> ";1H")
  putStr "*"
  hFlush stdout

moveDown :: Int -> IO ()
moveDown whereTo = do
  putStr ("\ESC[" <> show (whereTo -1) <> ";1H")
  putStr " "
  putStr ("\ESC[" <> show whereTo <> ";1H")
  putStr "*"
  hFlush stdout

readKey :: Int -> Int -> Int -> IO Int
readKey initLine maxOption current = do
  -- putStr ("\ESC[" <> show ((initLine - maxOption - 1) + current) <> ";1H")
  putStr ("\ESC[" <> show initLine <> ";1H")
  hFlush stdout
  k <- getKey
  case k of
    "\ESC[A" -> do -- putStr "↑"
      let next = current - 1
      if current == 1 then readKey initLine maxOption current else do
        moveUp ((initLine - maxOption - 1) + next)
        readKey initLine maxOption next
    "\ESC[B" -> do -- putStr "↓"
      let next = current + 1
      if current == maxOption then readKey initLine maxOption current else do
        moveDown ((initLine - maxOption - 1) + next)
        readKey initLine maxOption next
    "\LF" -> do -- "\n"
      -- putStr "RETURN"
      return current
    c        -> do
      putStr c
      readKey initLine maxOption current
    -- "\DEL"   -> putStr "⎋"

printOptions :: Options -> IO ()
printOptions opts = do
  let items = zip [1::Int .. length (opts.items) + 1] opts.items
  for_ items $ \(i :: Int, c :: String) -> do
    let line = if i == opts.defaultChoice then "* " <> show i <> " " <> c else "  " <> show i <> " " <> c
    putStrLn line

getCurrentLineSequence :: IO String
getCurrentLineSequence = do
  bracket (hGetBuffering stdin) (hSetBuffering stdin) $ \_ -> do
    hSetBuffering stdin NoBuffering
    bracket (hGetEcho stdin) (hSetEcho stdin) $ \_ -> do
      hSetEcho stdin False
      putStr "\ESC[6n"
      hFlush stdout
      
      show <$> getKey

getCurrentLineNumber :: IO Int
getCurrentLineNumber = do
  c <- getCurrentLineSequence
  if not $ "\"\\ESC[" `isPrefixOf` c then return 404
  else case elemIndex ';' c of
    Just n -> do
      let (line, _) = splitAt n c
      -- putStrLn (drop 6 line)
      return (read (drop 6 line) :: Int)
    Nothing -> return 405
  -- return 1

main :: IO ()
main = do

  -- c <-  getCurrentLineSequence
  -- putStrLn c
  l <- getCurrentLineNumber
  putStrLn $ show l
  -- hFlush stdout
  putStrLn "Hello"
  printOptions options

  num <- upDownArrowsEntry l options.defaultChoice (length options.items)
  
  putStr ("\ESC[" <> show l <> ";1H")


  putStrLn $ show num

upDownArrowsEntry :: Int -> Int -> Int -> IO Int
upDownArrowsEntry currentLine currentChoice maxChoice = do
  bracket (hGetBuffering stdin) (hSetBuffering stdin) $ \_ -> do
    hSetBuffering stdin NoBuffering
    bracket (hGetEcho stdin) (hSetEcho stdin) $ \_ -> do
      hSetEcho stdin False
      readKey currentLine maxChoice currentChoice

data DefaultPrompt t
  = DefaultPrompt t
  | OptionalPrompt
  | MandatoryPrompt
  deriving (Eq, Functor)
  
-- promptList
--     :: String
--       -- ^ prompt
--     -> [String]
--       -- ^ choices
--     -> DefaultPrompt String
--       -- ^ optional default value
--     -> Maybe (String -> String)
--       -- ^ modify the default value to present in-prompt
--       -- e.g. empty string maps to "(none)", but only in the
--       -- prompt.
--     -> Bool
--       -- ^ whether to allow an 'other' option
--     -> IO String



-- promptList msg choices def modDef hasOther = do
--   printChoices choices
--   putStrLn $ msg ++ ":"
--   return ""

-- printChoices :: [String] -> DefaultPrompt String -> IO ()
-- printChoices opts defaultPrompt = do
--   let items = zip [1::Int .. length opts + 1] opts
--   let choicePrinter = 
    
--   for_ items $ \(i :: Int, c :: String) -> do
--     let line = if c == opts.defaultChoice then "* " <> show i <> " " <> c else "  " <> show i <> " " <> c
--     putStrLn line