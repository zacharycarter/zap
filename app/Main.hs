{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import qualified Data.Text.IO as TIO
import System.Environment
import System.Exit

import Zap.Compiler

main :: IO ()
main = do
  args <- getArgs
  case args of
    [input, output] -> do
      source <- TIO.readFile input
      case compile' defaultCompileOptions source of
        Left err -> do
          putStrLn $ "Compilation failed: " ++ show err
          exitFailure
        Right result -> do
          case generatedCode result of
            Just cCode -> do
              TIO.writeFile output cCode
              putStrLn $ "Successfully compiled " ++ input ++ " to " ++ output
            Nothing -> putStrLn $ "Code generation failed"
    _ -> do
      putStrLn "Usage: zap <input.zap> <output.c>"
      exitFailure
