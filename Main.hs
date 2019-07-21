module Main where

import System.IO
import System.IO.Unsafe
import System.Directory
import System.Environment
import Control.Monad
import Control.Conditional
import System.Console.ANSI

getLinkText :: FilePath -> String
getLinkText path = " -> " ++ (unsafePerformIO $ getSymbolicLinkTarget path)

printList :: Color -> FilePath -> [FilePath] -> IO ()
printList color _ [] = return ()
printList color dir files = do
  -- path directory
  setSGR [ SetColor Foreground Vivid White ]
  putStr dir >> putStr "/"
  -- file nam
  setSGR [ SetColor Foreground Vivid color ]
  putStr (head files)
  -- symlink arrow
  setSGR [ SetColor Foreground Vivid White ]
  let fullPath = dir ++ "/" ++ head files
  let isLink = (unsafePerformIO . pathIsSymbolicLink) $ fullPath
  cond[(isLink, putStr $ getLinkText fullPath), (otherwise, return ())]

  putStrLn ""
  printAll dir (tail files)

printAll :: FilePath -> [FilePath] -> IO ()
printAll dir items = do
  printList Cyan dir (map (++ "/") dirs)
  printList Green dir files
        where files = filter (unsafePerformIO . doesFileExist) items
              dirs = filter (unsafePerformIO . doesDirectoryExist) items
              
main :: IO ()
main = do
  putStrLn ""
  --getArgs >>= print
  listDirectory "." >>= printAll "."
  putStrLn ""
    