{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Zap.Integration.Runner
  ( runTest
  , runTests
  , compileCCode
  , runCompiledProgram
  ) where

import Control.Monad (forM)
import System.IO
import System.Process
import System.Exit
import System.Directory
import Control.Exception (bracket, catch)
import Text.Printf
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Zap.Compiler
import Zap.Integration.Types

-- | Run all integration tests and report results
runTests :: [TestCase] -> IO Bool
runTests tests = do
  printf "Running %d integration tests\n" (length tests)
  results <- forM tests $ \test -> do
    printf "Test: %s..." (testName test)
    hFlush stdout
    result <- runTest test
    case result of
      TestSuccess -> do
        putStrLn " PASS"
        return True
      TestFailure msg -> do
        putStrLn " FAIL"
        putStrLn msg
        return False
      TestError msg -> do
        putStrLn " ERROR"
        putStrLn msg
        return False
      CompilationFailure msg -> do
        putStrLn " COMPILATION FAILED"
        putStrLn msg
        return False
      RuntimeFailure exitCode msg -> do
        putStrLn $ " RUNTIME FAILED (exit code: " ++ show exitCode ++ ")"
        putStrLn msg
        return False

  let passed = length $ filter id results
  let total = length tests
  printf "\nPassed %d of %d tests\n" passed total
  return $ passed == total

-- | Run single integration test and report results
runTest :: TestCase -> IO TestResult
runTest test = do  -- Keep the test parameter
  -- First compile Zap code to C
  let cr = compile defaultCompileOptions (sourceCode test)
  case (cr, expectedExitCode test) of  -- Add test parameter here
    -- For expected failures, we're done
    (Left _, ExitFailure _) -> return TestSuccess
    (Right _, ExitFailure _) -> return $ TestFailure "Compilation succeeded when failure was expected"
    (Left err, ExitSuccess) -> return $ TestFailure $ "Compilation failed unexpectedly: " ++ show err

    -- For expected successes, continue with C compilation
    (Right result, ExitSuccess) -> case generatedCode result of
      Nothing -> return $ TestFailure "No code generated when success was expected"
      Just cCode -> do
        -- Compile C code
        cr' <- compileCCode cCode
        case cr' of
          Left err -> return $ CompilationFailure err
          Right execPath -> do
            -- Run the compiled program
            (exitCode, output) <- runCompiledProgram execPath
            -- Clean up
            removeFile execPath `catch` \(_ :: IOError) -> return ()

            -- Check results (add test parameter references)
            if exitCode /= expectedExitCode test
              then return $ RuntimeFailure exitCode (T.unpack output)  -- Convert Text to String
              else if output == expectedOutput test
                then return TestSuccess
                else return $ TestFailure $
                  "Output mismatch:\nExpected: " ++
                  T.unpack (expectedOutput test) ++  -- Add test parameter
                  "\nActual: " ++ T.unpack output

-- | Write C code to a temp file and compile it
compileCCode :: T.Text -> IO (Either String FilePath)
compileCCode code = withTempFile "test" ".c" $ \sourcePath sourceHandle -> do
  -- Write C code to temp file
  TIO.hPutStr sourceHandle code
  hClose sourceHandle

  -- Generate output path
  let op = sourcePath ++ ".out"

  -- Run gcc with all warnings enabled
  (exitCode, out, err) <- readProcessWithExitCode "gcc"
    ["-Wall", sourcePath, "-o", op] ""

  case exitCode of
    ExitSuccess -> return $ Right op
    ExitFailure n -> return $ Left $
      "gcc failed with exit code " ++ show n ++
      "\nstdout: " ++ out ++
      "\nstderr: " ++ err

-- | Run a compiled program and capture its output
runCompiledProgram :: FilePath -> IO (ExitCode, T.Text)
runCompiledProgram path = do
  (exitCode, out, err) <- readProcessWithExitCode path [] ""
  return (exitCode, T.pack $ out ++ err)

-- | Create a temporary file that is deleted after use
withTempFile :: String -> String -> (FilePath -> Handle -> IO a) -> IO a
withTempFile prefix suffix action = do
  systemTempDir <- getTemporaryDirectory
  bracket
    (openTempFile systemTempDir (prefix ++ suffix))
    (\(path, handle) -> do
      hClose handle  -- Close handle if not already closed
      removeFile path `catch` \(_ :: IOError) -> return ())
    (uncurry action)
