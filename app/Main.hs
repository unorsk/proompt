{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where
import Data.Foldable (for_)
import System.IO (stdin, hReady, hGetEcho, hSetEcho, hGetBuffering, hSetBuffering, BufferMode (NoBuffering), hFlush, stdout, )
import Control.Exception.Base (bracket)
import Data.List (isPrefixOf, elemIndex)
import Data.Maybe (fromMaybe)

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
  putStr "\ESC[s"--today
  hFlush stdout
  k <- getKey
  case k of
    "\ESC[A" -> do -- putStr "↑"
      let next = if current == 0 then 1 else current - 1 -- 
      if current == 1 then readKey initLine maxOption current else do
        moveUp ((initLine - maxOption - 1) + next)
        putStr "\ESC[u"
        readKey initLine maxOption next
    "\ESC[B" -> do -- putStr "↓"
      let next = current + 1
      if current == maxOption then readKey initLine maxOption current else do
        moveDown ((initLine - maxOption - 1) + next)
        putStr "\ESC[u"
        readKey initLine maxOption next
    "\LF" -> do -- "\n"
      putStr "\ESC[u"
      return current
    c        -> do
      putStr "\ESC[u"
      putStr c
      readKey initLine maxOption current
    -- "\DEL"   -> putStr "⎋"

printOptions :: [String] -> DefaultPrompt String -> Maybe (String -> String) -> IO ()
printOptions opts defaultChoice modDef = do
  let items = zip [1::Int .. length opts + 1] opts
  for_ items $ \(i :: Int, val :: String) -> do
    let c = fromMaybe id modDef val
    case defaultChoice of
      DefaultPrompt defChoice -> do
          let line = if val == defChoice then "* " <> show i <> " " <> c else "  " <> show i <> " " <> c
          putStrLn line
      OptionalPrompt -> putStrLn $ "  " <> show i <> " " <> c
      --MandatoryPrompt
      _ -> putStrLn $ "  " <> show i <> " " <> c

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
  result <- promptList "Hello: " ["Option one", "Option two", "Option three"] (DefaultPrompt "Option two") (Just (\c -> if c == "Option two" then "NONE" else c)) False
  -- result <- promptList "Hello!" ["Option one", "Option two", "Option three"] OptionalPrompt  Nothing False
  putStr result


upDownArrowsEntry :: Int -> Int -> Int -> IO Int
upDownArrowsEntry currentLine currentChoice maxChoice = do
  bracket (do
    putStr "\ESC[s"
    hGetBuffering stdin) (\bufferMode -> do
      hSetBuffering stdin bufferMode
      putStr "\ESC[u"
      ) $ \_ -> do
    hSetBuffering stdin NoBuffering
    bracket (do
      hGetEcho stdin) (\isEcho -> do
        hSetEcho stdin isEcho
        ) $ \_ -> do
      hSetEcho stdin False
      readKey currentLine maxChoice currentChoice

data DefaultPrompt t
  = DefaultPrompt t
  | OptionalPrompt
  | MandatoryPrompt
  deriving (Eq, Functor)
  
promptList
    :: String
      -- ^ prompt
    -> [String]
      -- ^ choices
    -> DefaultPrompt String
      -- ^ optional default value
    -> Maybe (String -> String)
      -- ^ modify the default value to present in-prompt
      -- e.g. empty string maps to "(none)", but only in the
      -- prompt.
    -> Bool
      -- ^ whether to allow an 'other' option
    -> IO String
promptList msg choices def modDef hasOther = do
  printOptions choices def modDef
  putStr $ msg ++ ":"
  -- TODO determine default index based on  DefaultPrompt maybe?
  let defaultIndex = 0
  l <- getCurrentLineNumber
  n <- upDownArrowsEntry l defaultIndex (length options.items)
  return (choices!!(n-1))
