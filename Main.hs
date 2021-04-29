module Main where

import System.Directory
import System.Process
import System.Console.Readline
import System.IO
import Data.List
import Control.Exception
import Text.Read
import Data.List.Split

getInput    :: [String] -> IO String
handleInput :: String   -> [String]  -> IO ()
play        :: [String] -> IO ()
file        :: String   -> IO ()
dele        :: String   -> IO ()
download    :: [String] -> IO ()
loop        :: IO ()
main        :: IO ()

getInput options = do
  putStrLn (duplicate "-" 15)
  recursivePrint options 0
  putStrLn (duplicate "-" 15)
  maybeLine <- readline "-> "
  case maybeLine of
    Just line -> do return line
    Nothing   -> do return ""
  where
    duplicate      :: String   -> Int -> String
    recursivePrint :: [String] -> Int -> IO ()
    duplicate string n = concat $ replicate n string
    recursivePrint (option:options) control = do
      putStrLn (show control ++ ": " ++ option)
      recursivePrint options (control + 1)
    recursivePrint [] control = return ()

handleInput input options = do
  callCommand "clear"
  if null input then do
    current_path  <- getCurrentDirectory
    let split_dir =  splitOn "/" current_path
    setCurrentDirectory (intercalate "/" (init split_dir))
  else
    case readMaybe input :: Maybe Int of
      Just input -> do print input ; setCurrentDirectory (options!!input)
      Nothing    -> do checkInput input
    where
      checkInput :: String -> IO ()
      checkInput input | head (splitOn "play" input)     == "" = do play (splitOn "," (splitOn "play" input!!1)) -- oh god this is (less awful than it was) awful
                       | head (splitOn "file" input)     == "" = do file (splitOn "file" input!!1)
                       | head (splitOn "del"  input)     == "" = do dele (splitOn "del"  input!!1)
                       | head (splitOn "download" input) == "" = do download (splitOn "," (splitOn "download" input!!1))
                       | otherwise                             = do putStrLn "if you see this you did something wrong"

play [] = return ()
play (file:files) = do
  current_path <- getCurrentDirectory
  files_       <- listDirectory current_path
  callCommand ("mpv \'" ++ files_!!(read file::Int) ++ "\'")
  play files

file fileName = do
  let newName   = splitOn " " (fileName ++ " ")
  let finalName = intercalate "-" newName
  callCommand ("mkdir \'" ++ tail (init finalName) ++ "\'")

dele fileName = do
  current_path <- getCurrentDirectory
  files_       <- listDirectory current_path
  callCommand ("rm -rf \'" ++ files_!!(read fileName::Int) ++ "\'")

download [] = return ()
download (search:searches) = do
  callCommand ("youtube-dl \"ytsearch1:" ++ search ++ "\"")
  download searches

loop = do
  current_path <- getCurrentDirectory
  files        <- listDirectory current_path
  input        <- getInput files
  handleInput input files
  loop

main = do
  hSetBuffering stdout NoBuffering
  setCurrentDirectory "/home/henry/Music"
  loop
