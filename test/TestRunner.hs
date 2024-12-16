-- {-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE ScopedTypeVariables #-}
-- module TestRunner
--   ( compileAndRun
--   ) where

-- import qualified Data.Text as T
-- import qualified Data.Text.IO as TIO
-- import System.Process
-- import System.Directory
-- import System.FilePath
-- import System.Exit
-- import Control.Monad.IO.Class
-- import Control.Exception (bracket_, catch, SomeException)

-- import Compiler

-- data TestError
--   = CompilerError CompileError
--   | GccError String
--   | RuntimeError String
--   deriving (Show, Eq)

-- compileAndRun :: T.Text -> IO (Either TestError T.Text)
-- compileAndRun source = do
--   tmpDir <- getTemporaryDirectory
--   let cFile = tmpDir </> "test.c"
--   let exeFile = tmpDir </> "test"

--   bracket_
--     (return ())
--     (removeFile cFile `catch` (\(_ :: SomeException) -> return ()) >>
--      removeFile exeFile `catch` (\(_ :: SomeException) -> return ()))
--     $ do
--       case compile source of
--         Left err -> return $ Left $ CompilerError err
--         Right cCode -> do
--           TIO.writeFile cFile cCode
--           (gccExit, _, gccErr) <- readProcessWithExitCode "gcc" [cFile, "-o", exeFile] ""
--           case gccExit of
--             ExitFailure _ -> return $ Left $ GccError gccErr
--             ExitSuccess -> do
--               (exit, stdout, stderr) <- readProcessWithExitCode exeFile [] ""
--               case exit of
--                 ExitSuccess -> return $ Right $ T.pack stdout
--                 ExitFailure _ -> return $ Left $ RuntimeError stderr
module TestRunner () where
