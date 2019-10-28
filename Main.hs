module Main where

import           Control.Conditional

import           Data.List
import           System.Console.ANSI
import qualified System.Directory    as Dir

columns :: [(String, Color)] -> IO ()
columns = undefined

data Item
  = File String
  | Directory String
  deriving (Show)

cmpItems :: Item -> Item -> Ordering
cmpItems (Directory a) (File b)      = LT
cmpItems (File a) (Directory b)      = GT
cmpItems (File a) (File b)           = compare a b
cmpItems (Directory a) (Directory b) = compare a b

statItem :: FilePath -> IO Item
statItem path =
  condM
    [ (Dir.doesDirectoryExist path, return $ Directory path)
    , (Dir.doesFileExist path, return $ File path)
    ]

printColored :: (String, Color) -> IO ()
printColored (str, col) = do
  setSGR [SetColor Foreground Vivid col]
  putStrLn str

color :: Item -> (String, Color)
color (File item)      = (item, Red)
color (Directory item) = (item ++ "/", Green)

printItems :: [Item] -> IO ()
printItems items = mapM_ (printColored . color) $ sortBy cmpItems items

printDirContents :: FilePath -> IO ()
printDirContents dir = Dir.listDirectory dir >>= mapM statItem >>= printItems

main :: IO ()
main = printDirContents "."
