module Main where

import Parser

import Control.Monad.Trans
import System.Console.Haskeline

process :: String -> IO ()
process input = do
  let res = parse_prog input
  case res of
    Left err -> print err
    Right re -> mapM_ print re

main :: IO ()
main = runInputT defaultSettings loop
  where loop = do
          minput <- getInputLine "ready>"
          case minput of
            Nothing -> outputStrLn "End."
            Just input -> (liftIO $ process input) >> loop
