module Main where

import           Control.Conditional
import           Control.Monad
import           Data.Function
import           Data.List
import           Flow
import           System.Console.ANSI
import           System.Directory
import           System.Environment
import           System.IO
import           System.IO.Unsafe
import           System.Posix.IO       (stdOutput)
import           System.Posix.Terminal (queryTerminal)
import           Text.Printf

instance PrintfArg Bool where
  formatArg True _ s  = "true" ++ s
  formatArg False _ s = "false" ++ s

-- data Args = ShowAll | ShowAlmostAll | ShowDefault | HumanFormat | LongListing deriving (Enum)
data ShowMode
  = ShowAll
  | ShowAlmostAll
  | ShowDefault
  deriving (Enum, Show)

data Args =
  Args
    { showMode    :: ShowMode
    , humanFormat :: Bool
    , longListing :: Bool
    }
  deriving (Show)

parseShortArg :: String -> String
parseShortArg ('l':tail) = "(long)" ++ parseShortArg tail
parseShortArg ('h':tail) = "(human)" ++ parseShortArg tail
parseShortArg _          = ""

parseLongArg :: String -> String
parseLongArg "human-readable" = "(human)"
parseLongArg _                = "(longarg)"

parseArg :: String -> String
parseArg ""             = ""
parseArg ('-':'-':tail) = parseLongArg tail
parseArg ('-':tail)     = parseShortArg tail
parseArg a              = "uff"

parseArgs :: [String] -> String
parseArgs (head:args) = parseArg head ++ parseArgs args
parseArgs _           = ""

isTty :: Bool
{-# NOINLINE isTty #-}
isTty = unsafePerformIO (queryTerminal stdOutput)

getLinkText :: FilePath -> String
getLinkText path = " -> " ++ unsafePerformIO (getSymbolicLinkTarget path)

printFiles :: Color -> FilePath -> [FilePath] -> IO ()
printFiles color _ [] = return ()
printFiles color dir files
  -- file name
 = do
  setSGR [SetColor Foreground Vivid color]
  putStr (head files)
  -- symlink arrow
  setSGR [SetColor Foreground Vivid White]
  let fullPath = dir ++ "/" ++ head files
  let isLink = (unsafePerformIO . pathIsSymbolicLink) fullPath
  cond [(isLink, putStr $ getLinkText fullPath), (otherwise, return ())]
  putStrLn ""
  printFiles color dir (tail files)

printAll :: FilePath -> [FilePath] -> IO ()
printAll dir items = do
  filterM doesDirectoryExist items >>=
    map (++ "/") .> sort .> printFiles Green dir
  filterM doesFileExist items >>= sort .> printFiles Cyan dir

main :: IO ()
main = do
  printf "isTty:%b" isTty
  putStrLn "\n"
  putStrLn . parseArgs . unsafePerformIO $ getArgs
  --getArgs >>= print
  listDirectory "." >>= printAll "."
  putStrLn ""
