{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Text.IO as TIO
import System.Environment
import System.FilePath
import System.Exit
import Control.Monad

import Zap.Compiler

main :: IO ()
main = do
  putStrLn "Usage: zap <input.zap> <output.c>"
  -- args <- getArgs
  -- case args of
  --   [input, output] -> do
  --     source <- TIO.readFile input
  --     case compile source of
  --       Left err -> do
  --         putStrLn $ "Compilation failed: " ++ show err
  --         exitFailure
  --       Right cCode -> do
  --         TIO.writeFile output cCode
  --         putStrLn $ "Successfully compiled " ++ input ++ " to " ++ output
  --   _ -> do
  --     putStrLn "Usage: zap <input.zap> <output.c>"
  --     exitFailure
