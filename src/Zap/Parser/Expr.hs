{-# LANGUAGE OverloadedStrings #-}

module Zap.Parser.Expr
  ( ExprParser (..),
    defaultExprParser,
    mapLexError,
    parseAssign,
    parseBlockExprs,
    parseExpr,
    parseExprFromText,
    parseExpression,
    parseLetBinding,
    parsePrintStatement,
    parseSingleBindingLine,
    parseBlockImpl,
    parseFuncDecl,
    parseVarDecl,
    parseWhileExpr,
    parseMaybeCall,
    parseTypeParams,
    isPrint,
    isStringLit,
    isValidName,
    isOperator,
  )
where

import Control.Monad (when)
import Control.Monad.Error.Class
import Control.Monad.State
import Data.Char (isUpper)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Debug.Trace
import Zap.AST
import Zap.Analysis.Lexical
import Zap.Parser.Core
import Zap.Parser.Types

data ExprParser = ExprParser
  { parseBasicExpr :: Parser Expr,
    parseBlock :: Parser Expr,
    parseBreak :: Parser Expr,
    parseResult :: Parser Expr
  }

-- Token validation helpers
isValidName :: Token -> Bool
isValidName (TWord name) = do
  let nameHd = case name of
        [] -> '_'
        hd : _ -> hd
  traceShow ("Validating name: " ++ name) $
    not (name `elem` ["break", "result", "block", "let", "var", "fn", "print", "if", "else", "while"])
      && not (null name)
      && (isAlpha nameHd)
isValidName _ = False

isBlockKeyword :: Token -> Bool
isBlockKeyword (TWord "block") = True
isBlockKeyword _ = False

isColon :: Token -> Bool
isColon TColon = True
isColon _ = False

isBreak :: Token -> Bool
isBreak (TWord "break") = True
isBreak _ = False

isPrint :: Token -> Bool
isPrint (TWord "print") = True
isPrint _ = False

isResult :: Token -> Bool
isResult (TWord "result") = True
isResult _ = False

isStringLit :: Token -> Bool
isStringLit (TString _) = True
isStringLit _ = False

isNumber :: Token -> Bool
isNumber (TNumber _) = True
isNumber _ = False

isOperator :: Token -> Bool
isOperator (TOperator op) = op `elem` ["+", "-", "*", "/", "<", ">", "=", "&", "|", "+="]
isOperator _ = False

isVecConstructor :: Token -> Bool
isVecConstructor (TVec _) = True
isVecConstructor _ = False

-- Main expression parser
parseExpr :: ExprParser -> Parser Expr
parseExpr parsers = do
  st <- get
  traceM $ "parseExpr state: " ++ show (stateIndent st) ++ " tokens: " ++ show (take 3 $ stateTokens st)
  case stateTokens st of
    (tok : _) -> do
      traceM $ "Parsing expression starting with: " ++ show (locToken tok)
      case locToken tok of
        TWord "let" -> parseLetBinding parsers
        TWord "block" -> parseBlock parsers
        TWord "break" -> parseBreak parsers
        TWord "result" -> parseResult parsers
        TVec _ -> parseVectorLiteral
        TWord "print" -> parsePrintStatement
        TWord "var" -> parseVarDecl
        _ -> parseExpression
    [] -> throwError $ EndOfInput "expression"

-- Top level expression parser that handles operator precedence
parseExpression :: Parser Expr
parseExpression = parseExpressionWithType Nothing

parseExpressionWithType :: Maybe Type -> Parser Expr
parseExpressionWithType expectedType = do
  traceM "Parsing complete expression"
  st <- get
  case stateTokens st of
    (tok : _)
      | locToken tok == TWord "if" -> parseIf expectedType -- Already exists
      | locToken tok == TWord "while" -> parseWhileExpr expectedType -- Already exists
      -- New cases handled at top level
      | locToken tok == TWord "else" ->
          throwError $ UnexpectedToken tok "'if' before 'else'" -- Better error
      | otherwise -> parseAssign expectedType >>= remainingExpression expectedType
    _ -> throwError $ EndOfInput "expression"

parseIf :: Maybe Type -> Parser Expr
parseIf expectedType = do
  traceM "\n=== parseIf ==="
  traceM "Parsing if statement"
  st <- get
  traceM $ "Current tokens: " ++ show (take 5 $ stateTokens st)

  _ <- matchToken isIf "if"
  condition <- parseExpressionWithType expectedType
  _ <- matchToken isColon ":"

  traceM "Parsing then branch"
  -- Parse then branch with proper indentation
  let curIndent = stateIndent st
  let newIndent = curIndent + 2
  modify $ \s -> s {stateIndent = newIndent}

  thenStmts <- parseBlockExprs expectedType BasicBlock newIndent

  traceM $ "Then branch parsed: " ++ show thenStmts
  traceM "Looking for else branch"

  let thenBlock = Block "if_then" (fst thenStmts) (snd thenStmts)

  -- Reset indentation for else check
  modify $ \s -> s {stateIndent = curIndent}
  st' <- get
  traceM $ "Current tokens after then: " ++ show (take 3 $ stateTokens st')

  -- Look for else branch at original indent level
  elseBlock <- do
    case stateTokens st' of
      (tok : _) | isElse (locToken tok) -> do
        traceM "Found else branch"
        _ <- matchToken (\t -> t == TWord "else") "else"
        _ <- matchToken isColon ":"

        -- Parse else branch with proper indentation
        modify $ \s -> s {stateIndent = newIndent}
        elseStmts <- parseBlockExprs expectedType BasicBlock newIndent
        modify $ \s -> s {stateIndent = curIndent}
        return $ Block "if_else" (fst elseStmts) (snd elseStmts)
      _ -> return $ Block "if_else" [] Nothing

  return $ If condition thenBlock elseBlock

-- Helper for if keyword
isIf :: Token -> Bool
isIf (TWord "if") = True
isIf _ = False

isElse :: Token -> Bool
isElse (TWord "else") = True
isElse _ = False

remainingExpression :: Maybe Type -> Expr -> Parser Expr
remainingExpression expectedType left = do
  st <- get
  traceM $ "Processing remaining expression with left: " ++ show left
  case stateTokens st of
    (tok : _) -> do
      traceM $ "Next token in remaining expression: " ++ show (locToken tok)
      case locToken tok of
        TOperator "*" -> do
          traceM "Found multiplication operator"
          _ <- matchToken isOperator "*"
          right <- parseAssign expectedType
          remainingExpression expectedType (BinOp Mul left right)
        TOperator "/" -> do
          traceM "Found division operator"
          _ <- matchToken isOperator "/"
          right <- parseAssign expectedType
          remainingExpression expectedType (BinOp Div left right)
        _ -> return left
    [] -> return left

parseAssign :: Maybe Type -> Parser Expr
parseAssign expectedType = do
  traceM "Parsing assignment"
  left <- parseComparison expectedType
  traceM $ "Parsed comparison: " ++ show left
  st <- get
  traceM $ "Expression state before operator check: " ++ show (take 3 $ stateTokens st)
  case stateTokens st of
    (tok : _) -> case locToken tok of
      TOperator "+=" -> do
        traceM $ "Found += operator at col " ++ show (locCol tok)
        _ <- matchToken (\t -> t == TOperator "+=") "+="
        right <- parseComparison expectedType
        case left of
          Var name -> do
            traceM $ "Creating += assignment for " ++ name
            return $ AssignOp name Add right
          _ -> do
            traceM $ "Invalid left side for +=: " ++ show left
            throwError $ UnexpectedToken tok "variable name before +="
      TOperator "=" -> do
        traceM $ "Found = operator at col " ++ show (locCol tok)
        _ <- matchToken (\t -> t == TOperator "=") "="
        right <- parseAssign expectedType
        case left of
          Var name -> do
            traceM $ "Creating = assignment for " ++ name
            return $ Assign name right
          _ -> do
            traceM $ "Invalid left side for =: " ++ show left
            throwError $ UnexpectedToken tok "variable name before ="
      _ -> do
        traceM $ "No assignment operator found, token was: " ++ show tok
        return left
    [] -> return left

parseComparison :: Maybe Type -> Parser Expr
parseComparison expectedType = do
  traceM "Parsing comparison expression"
  left <- parseAdditive expectedType
  traceM $ "Parsed additive: " ++ show left
  remainingComparison expectedType left
  where
    remainingComparison expectedType' left = do
      st <- get
      case stateTokens st of
        (tok : _) -> case locToken tok of
          TOperator "<" -> do
            traceM "Found less than operator"
            _ <- matchToken isOperator "<"
            right <- parseAdditive expectedType'
            remainingComparison expectedType' (BinOp Lt left right)
          TOperator ">" -> do
            traceM "Found greater than operator"
            _ <- matchToken isOperator ">"
            right <- parseAdditive expectedType'
            remainingComparison expectedType' (BinOp Gt left right)
          TOperator "==" -> do
            traceM "Found equality operator"
            _ <- matchToken (== TOperator "==") "=="
            right <- parseAdditive expectedType'
            remainingComparison expectedType' (BinOp Eq left right)
          _ -> return left
        [] -> return left

-- Parse addition and subtraction with proper precedence
parseAdditive :: Maybe Type -> Parser Expr
parseAdditive expectedType = do
  traceM "Parsing additive expression"
  left <- parseMultiplicative expectedType
  remainingAdditive expectedType left
  where
    remainingAdditive expectedType' left = do
      st <- get
      traceM $ "Parsing remaining additive with left: " ++ show left
      case stateTokens st of
        (tok : _) -> case locToken tok of
          TOperator "+" -> do
            traceM "Found addition operator"
            _ <- matchToken isOperator "+"
            right <- parseMultiplicative expectedType'
            remainingAdditive expectedType' (BinOp Add left right)
          TOperator "+=" -> do
            traceM "Found += operator"
            _ <- matchToken (\t -> t == TOperator "+=") "+="
            right <- parseMultiplicative expectedType'
            case left of
              Var name -> return $ AssignOp name Add right
              _ -> throwError $ UnexpectedToken tok "variable for assignment"
          TOperator "-" -> do
            traceM "Found subtraction operator"
            _ <- matchToken isOperator "-"
            right <- parseMultiplicative expectedType'
            remainingAdditive expectedType' (BinOp Sub left right)
          _ -> return left
        [] -> return left

-- Parse multiplication and division
parseMultiplicative :: Maybe Type -> Parser Expr
parseMultiplicative expectedType = do
  traceM "Parsing multiplicative expression"
  left <- parseUnary expectedType
  remainingMultiplicative expectedType left
  where
    remainingMultiplicative expectedType' left = do
      st <- get
      case stateTokens st of
        (tok : _) -> case locToken tok of
          TOperator "*" -> do
            _ <- matchToken isOperator "*"
            right <- parseUnary expectedType'
            remainingMultiplicative expectedType' (BinOp Mul left right)
          TOperator "/" -> do
            _ <- matchToken isOperator "/"
            right <- parseUnary expectedType'
            remainingMultiplicative expectedType' (BinOp Div left right)
          TOperator "div" -> do
            _ <- matchToken (\t -> t == TOperator "div") "div"
            right <- parseUnary expectedType'
            remainingMultiplicative expectedType' (BinOp Div left right)
          _ -> return left
        [] -> return left

-- Parse unary operations and basic terms
parseUnary :: Maybe Type -> Parser Expr
parseUnary expectedType = parseTerm expectedType

-- Parse basic terms (numbers, variables, vector literals)
parseTerm :: Maybe Type -> Parser Expr
parseTerm expectedType = do
  traceM $ "Parsing term with expected type: " ++ show expectedType
  st <- get
  case stateTokens st of
    (tok : _) -> do
      traceM $ "Parsing term starting with token: " ++ show tok
      case locToken tok of
        TNumber n -> do
          traceM $ "Found number token: " ++ n
          _ <- matchToken isNumber "number"
          -- Look ahead for type suffix
          st' <- get
          case stateTokens st' of
            (suffixTok : _) | isTypeSuffix (locToken suffixTok) -> do
              traceM $ "Found type suffix after number"
              _ <- matchToken isTypeSuffix "type suffix"
              let typ = case locToken suffixTok of
                    TTypeSuffix s -> parseNumType s
                    _ -> error "Impossible: non-suffix token passed isTypeSuffix"
              traceM $ "Parsed number with explicit suffix type: " ++ show typ
              buildNumericLiteral typ n
            _ -> do
              traceM $ "No type suffix found, using expected type: " ++ show expectedType
              -- If we have an expected type, use it for the literal
              let inferredType = case expectedType of
                    Just (TypeNum t) -> t -- Use the expected numeric type
                    _ ->
                      if '.' `elem` n
                        then Float64
                        else Int32 -- Default to Int32 when no expected type
              traceM $ "Inferred type: " ++ show inferredType
              buildNumericLiteral inferredType n
          where
            buildNumericLiteral :: NumType -> String -> Parser Expr
            buildNumericLiteral typ val = do
              traceM $ "Building numeric literal with type " ++ show typ
              let expr = case typ of
                    Float32 -> Lit (FloatLit val (Just Float32))
                    Float64 -> Lit (FloatLit val (Just Float64))
                    Int32 -> Lit (IntLit val (Just Int32))
                    Int64 -> Lit (IntLit val (Just Int64))
              parseFieldOrCallChain expr
        TLeftParen -> do
          traceM "Found opening parenthesis"
          _ <- matchToken (== TLeftParen) "("
          expr <- parseExpression
          traceM $ "Parsed expression in parentheses: " ++ show expr
          _ <- matchToken (== TRightParen) ")"
          -- Continue parsing any following operators
          parseFieldOrCallChain expr
        TWord name | isValidName (locToken tok) -> do
          traceM $ "Found identifier: " ++ name
          _ <- matchToken isValidName "identifier"
          parseFieldOrCallChain (Var name)
        TWord "print" -> parsePrintStatement
        TWord "var" -> parseVarDecl
        _ -> parseBaseTerm >>= parseFieldAccess
    [] -> throwError $ EndOfInput "term"

parseFieldOrCallChain :: Expr -> Parser Expr
parseFieldOrCallChain expr = do
  traceM "\n=== parseFieldOrCallChain ==="
  traceM $ "Starting with expr: " ++ show expr
  st <- get
  traceM $ "Next tokens: " ++ show (take 3 $ stateTokens st)
  case stateTokens st of
    (tok : _) -> do
      let t = locToken tok
      traceM $ "Processing token: " ++ show t
      case t of
        TLeftBracket -> do
          case expr of
            Var name -> do
              traceM $ "\n=== Processing type parameters for: " ++ name
              typeParams <- parseTypeParams

              -- Parse type parameters regardless of struct existence
              paramTypes <-
                mapM
                  ( \p -> case p of
                      "i32" -> return $ TypeNum Int32
                      "i64" -> return $ TypeNum Int64
                      "f32" -> return $ TypeNum Float32
                      "f64" -> return $ TypeNum Float64
                      _ -> throwError $ UnexpectedToken undefined "valid type"
                  )
                  typeParams

              -- Build specialized name
              let paramSuffixes = map typeToSuffix paramTypes
              let specializedName = name ++ "_" ++ T.unpack (T.intercalate "_" (map T.pack paramSuffixes))

              -- Check struct definition and update symbol table if it exists
              st' <- get
              case M.lookup name (structNames (stateSymTable st')) of
                Just sid -> case lookupStruct sid (stateSymTable st') of
                  Just baseDef -> do
                    traceM $ "Registering specialized struct: " ++ show specializedName
                    let (_, newSymTable) =
                          registerSpecializedStruct specializedName baseDef paramTypes (stateSymTable st')
                    traceM $ "Updated symbol table: " ++ show newSymTable
                    modify $ \s -> s {stateSymTable = newSymTable}
                  Nothing -> return ()
                Nothing -> return ()

              -- Continue with constructor call
              parseMaybeCall specializedName
            Let _ _ -> throwError $ UnexpectedToken tok "expected variable name before type parameters"
            Block _ _ _ -> throwError $ UnexpectedToken tok "expected variable name before type parameters"
            Break _ _ -> throwError $ UnexpectedToken tok "expected variable name before type parameters"
            Result _ -> throwError $ UnexpectedToken tok "expected variable name before type parameters"
            BinOp _ _ _ -> throwError $ UnexpectedToken tok "expected variable name before type parameters"
            If _ _ _ -> throwError $ UnexpectedToken tok "expected variable name before type parameters"
            StructLit _ _ -> throwError $ UnexpectedToken tok "expected variable name before type parameters"
            FieldAccess _ _ -> throwError $ UnexpectedToken tok "expected variable name before type parameters"
            ArrayLit _ _ -> throwError $ UnexpectedToken tok "expected variable name before type parameters"
            Index _ _ -> throwError $ UnexpectedToken tok "expected variable name before type parameters"
            VarDecl _ _ -> throwError $ UnexpectedToken tok "expected variable name before type parameters"
            Assign _ _ -> throwError $ UnexpectedToken tok "expected variable name before type parameters"
            AssignOp _ _ _ -> throwError $ UnexpectedToken tok "expected variable name before type parameters"
            Call _ _ -> throwError $ UnexpectedToken tok "expected variable name before type parameters"
            While _ _ -> throwError $ UnexpectedToken tok "expected variable name before type parameters"
            Lit _ -> throwError $ UnexpectedToken tok "expected variable name before type parameters"
        TDot -> do
          _ <- matchToken (== TDot) "dot"
          fieldTok <- matchToken isValidName "field name"
          case locToken fieldTok of
            TWord field -> parseFieldOrCallChain (FieldAccess expr field)
            _ -> throwError $ UnexpectedToken fieldTok "field name"
        TLeftParen -> do
          _ <- matchToken (== TLeftParen) "("
          args <- parseCallArgs
          _ <- matchToken (== TRightParen) ")"
          case expr of
            Var name -> parseFieldOrCallChain (Call name args)
            _ -> throwError $ UnexpectedToken tok "expected a function name for the call"
        _ -> return expr
    [] -> return expr

parseTypeParams :: Parser [String]
parseTypeParams = do
  traceM "\n=== parseTypeParams ==="
  st <- get
  traceM $ "\n=== parseTypeParams ==="
  traceM $ "Current indent: " ++ show (stateIndent st)
  traceM $ "Next tokens: " ++ show (take 5 $ stateTokens st)
  case stateTokens st of
    (tok : _) | locToken tok == TLeftBracket -> do
      _ <- matchToken (== TLeftBracket) "["
      params <- parseParamList
      _ <- matchToken (== TRightBracket) "]"
      return params
    _ -> return [] -- No type parameters
  where
    -- Parse a comma-separated list of type parameters
    parseParamList = do
      -- Parse first parameter
      paramTok <- matchToken isValidName "type parameter"
      case locToken paramTok of
        TWord name -> do
          -- Check for comma indicating more parameters
          st <- get
          case stateTokens st of
            (next : _) | locToken next == TComma -> do
              _ <- matchToken (== TComma) ","
              -- Recursively parse rest of parameters
              rest <- parseParamList
              return (name : rest)
            _ -> return [name] -- Single parameter
        _ -> throwError $ UnexpectedToken paramTok "type parameter name"

parseBaseTerm :: Parser Expr
parseBaseTerm = do
  traceM "Parsing base term"
  st <- get
  case stateTokens st of
    (tok : _) -> case locToken tok of
      TLeftParen -> do
        traceM "Found opening parenthesis in base term"
        _ <- matchToken (== TLeftParen) "("
        expr <- parseExpression
        _ <- matchToken (== TRightParen) ")"
        traceM $ "Parsed parenthesized expression: " ++ show expr
        return expr
      TString s -> do
        traceM $ "Found string literal: " ++ s
        _ <- matchToken isStringLit "string literal"
        return $ Lit $ StringLit s
      TNumber n -> do
        traceM $ "Found numeric literal: " ++ n
        _ <- matchToken isNumber "number"
        if '.' `elem` n
          then return $ Lit (FloatLit n (Just Float32))
          else return $ Lit (IntLit n (Just Int32))
      TWord name | isValidName (locToken tok) -> do
        traceM $ "Found identifier: " ++ name
        _ <- matchToken isValidName "identifier"
        parseMaybeCall name
      TVec vecTypeStr -> do
        traceM $ "Found vector constructor"
        _ <- matchToken isVecConstructor "vector constructor"
        parseMaybeCall vecTypeStr
      _ -> throwError $ UnexpectedToken tok "term"
    [] -> throwError $ EndOfInput "term"

parseMaybeCall :: String -> Parser Expr
parseMaybeCall fname = do
  traceM $ "\n=== parseMaybeCall ==="
  traceM $ "Called with fname: " ++ fname
  st <- get
  traceM $ "Current tokens: " ++ show (take 5 $ stateTokens st)
  case stateTokens st of
    (tok : _) -> do
      traceM $ "First token: " ++ show tok
      case locToken tok of
        TLeftBracket -> do
          traceM $ "\n=== Processing type parameters for: " ++ fname
          _ <- matchToken (== TLeftBracket) "["

          -- Parse all type parameters recursively
          let parseTypeParamList = do
                paramTok <- matchToken isValidName "type name"
                case locToken paramTok of
                  TWord param -> do
                    -- Parse type from parameter
                    paramType <- case param of
                      "i32" -> return $ TypeNum Int32
                      "i64" -> return $ TypeNum Int64
                      "f32" -> return $ TypeNum Float32
                      "f64" -> return $ TypeNum Float64
                      _ -> throwError $ UnexpectedToken paramTok "valid type"

                    -- Check for comma indicating more parameters
                    st' <- get
                    case stateTokens st' of
                      (next : _) | locToken next == TComma -> do
                        _ <- matchToken (== TComma) ","
                        rest <- parseTypeParamList -- Recursive call
                        return (paramType : rest)
                      _ -> return [paramType]
                  _ -> throwError $ UnexpectedToken paramTok "type parameter"

          -- Parse all type parameters
          typeParams <- parseTypeParamList
          _ <- matchToken (== TRightBracket) "]"

          -- Get base struct info to validate parameter count
          case M.lookup fname (structNames $ stateSymTable st) of
            Just sid -> case lookupStruct sid (stateSymTable st) of
              Just def -> do
                when (length typeParams /= length (structParams def)) $
                  throwError $
                    UnexpectedToken tok $
                      "Wrong number of type parameters for " ++ fname

                -- Build specialized name from all parameters
                let paramSuffixes = map typeToSuffix typeParams
                let specializedName = fname ++ "_" ++ T.unpack (T.intercalate "_" (map T.pack paramSuffixes))

                -- Continue with constructor call
                _ <- matchToken (== TLeftParen) "("
                args <- parseCallArgs
                _ <- matchToken (== TRightParen) ")"
                return $ Call specializedName args
              Nothing -> throwError $ UnexpectedToken tok "valid struct definition"
            Nothing -> throwError $ UnexpectedToken tok "defined struct"
        TLeftParen -> do
          traceM "Found constructor/function call with parens"
          _ <- matchToken (== TLeftParen) "("

          -- Look up expected type from struct fields
          symTable <- gets stateSymTable
          let expectedType = case M.lookup fname (structNames symTable) of
                Just sid -> case lookupStruct sid symTable of
                  Just def -> case structFields def of
                    (_, TypeNum t) : _ -> Just (TypeNum t)
                    _ -> Nothing
                  Nothing -> Nothing
                Nothing -> Nothing

          args <- parseCallArgsWithType expectedType
          _ <- matchToken (== TRightParen) ")"
          return (Call fname args)
        TComma -> do
          traceM "Found constructor/function call with args"
          args <- parseCallArgs
          return (Call fname args)
        _ -> do
          traceM $ "No type params - returning var"
          return $ Var fname
    [] -> do
      traceM "No tokens - returning var"
      return $ Var fname

parseCallArgs :: Parser [Expr]
parseCallArgs = do
  st <- get
  case stateTokens st of
    [] -> throwError $ EndOfInput "expected tokens for arg parsing"
    (tok : _)
      | locToken tok == TRightParen -> return []
      | otherwise -> do
          -- Look up struct type for constructor calls
          let expectedType = case (stateTokens st) of
                (Located (TWord name) _ _ : _) | isConstructorName name -> do
                  let symTable = stateSymTable st
                  case M.lookup name (structNames symTable) of
                    Just sid -> case lookupStruct sid symTable of
                      Just def -> case structFields def of
                        (_, TypeNum t) : _ -> Just (TypeNum t)
                        _ -> Nothing
                      Nothing -> Nothing
                    Nothing -> Nothing
                _ -> Nothing
          firstArg <- parseExpressionWithType expectedType
          parseMoreArgs [firstArg]
  where
    isConstructorName :: String -> Bool
    isConstructorName "" = False
    isConstructorName (c : _) = isUpper c

    parseMoreArgs acc = do
      st <- get
      case stateTokens st of
        (tok : _) | locToken tok == TComma -> do
          _ <- matchToken (== TComma) "comma"
          arg <- parseExpression
          parseMoreArgs (acc ++ [arg])
        _ -> return acc

parseCallArgsWithType :: Maybe Type -> Parser [Expr]
parseCallArgsWithType expectedType = do
  st <- get
  case stateTokens st of
    [] -> throwError $ EndOfInput "expected tokens for arg list"
    (tok : _)
      | locToken tok == TRightParen -> return []
      | otherwise -> do
          firstArg <- parseExpressionWithType expectedType
          parseMoreArgs expectedType [firstArg]
  where
    parseMoreArgs :: Maybe Type -> [Expr] -> Parser [Expr]
    parseMoreArgs expectedType' acc = do
      st <- get
      case stateTokens st of
        (tok : _) | locToken tok == TComma -> do
          _ <- matchToken (== TComma) "comma"
          arg <- parseExpressionWithType expectedType'
          parseMoreArgs expectedType' (acc ++ [arg])
        _ -> return acc

parseFieldAccess :: Expr -> Parser Expr
parseFieldAccess expr = do
  traceM $ "\n=== parseFieldAccess ==="
  traceM $ "Base expression: " ++ show expr
  st <- get
  traceM $ "Current symbol table: " ++ show (stateSymTable st)
  case stateTokens st of
    (tok : _) | locToken tok == TDot -> do
      traceM "Found field access"
      _ <- matchToken (== TDot) "dot"
      fieldName <- matchToken isValidName "field name"
      case locToken fieldName of
        TWord name -> do
          traceM $ "Accessing field " ++ name
          -- Look up base type
          let baseType = case expr of
                Var varName -> lookupVarType varName (stateSymTable st)
                _ -> Nothing
          traceM $ "Base expression type: " ++ show baseType

          -- Create field access expression
          let nextExpr = FieldAccess expr name
          parseFieldAccess nextExpr
        _ -> throwError $ UnexpectedToken fieldName "field name"
    _ -> return expr

parseBasicExprImpl :: Parser Expr
parseBasicExprImpl = do
  traceM "Starting basic expression parse"
  st <- get
  checkIndent GreaterEq
  case stateTokens st of
    [] -> throwError $ EndOfInput "expression expected"
    (tok : _) -> case locToken tok of
      TString s -> do
        _ <- matchToken isStringLit "string literal"
        return $ Lit $ StringLit s
      TNumber n -> do
        traceM $ "Found number token: " ++ n
        _ <- matchToken isNumber "number"
        -- Look ahead for type suffix
        st' <- get
        case stateTokens st' of
          (suffixTok : _) | isTypeSuffix (locToken suffixTok) -> do
            traceM $ "Found type suffix: " ++ show suffixTok
            _ <- matchToken isTypeSuffix "type suffix"
            let typ = case locToken suffixTok of
                  TTypeSuffix s -> parseNumType s
                  _ -> error "Impossible: non-suffix token passed isTypeSuffix"
            case typ of
              Int32 -> return $ Lit (IntLit n (Just Int32))
              Int64 -> return $ Lit (IntLit n (Just Int64))
              Float32 -> return $ Lit (FloatLit n (Just Float32))
              Float64 -> return $ Lit (FloatLit n (Just Float64))
          _ -> do
            traceM "No type suffix found, defaulting to Int64/Float64"
            -- Default to 64-bit types
            let typ = if '.' `elem` n then Float64 else Int64
            case typ of
              Int32 -> return $ Lit (IntLit n (Just Int32))
              Int64 -> return $ Lit (IntLit n (Just Int64))
              Float32 -> return $ Lit (FloatLit n (Just Float32))
              Float64 -> return $ Lit (FloatLit n (Just Float64))
      TVec vecTypeStr -> do
        traceM $ "Parsing vector constructor: " ++ vecTypeStr
        _ <- matchToken isVecConstructor "vector constructor"
        -- Parse as a call
        parseMaybeCall vecTypeStr
      TWord name | isValidName (locToken tok) -> do
        _ <- matchToken isValidName "identifier"
        parseMaybeCall name
      _ -> throwError $ UnexpectedToken tok "term"

-- Helper to check for type suffix tokens
isTypeSuffix :: Token -> Bool
isTypeSuffix (TTypeSuffix _) = True
isTypeSuffix _ = False

-- Helper to convert type suffix to NumType
parseNumType :: String -> NumType
parseNumType "i32" = Int32
parseNumType "i64" = Int64
parseNumType "f32" = Float32
parseNumType "f64" = Float64
parseNumType t = error $ "Invalid numeric type suffix: " ++ t

parseLetBinding :: ExprParser -> Parser Expr
parseLetBinding _ = do
  traceM "Parsing let binding"
  _ <- matchToken (\t -> t == TWord "let") "let keyword"
  (varName, value) <- parseSingleBindingLine
  traceM $ "Completed let binding for: " ++ varName
  return $ Let varName value

-- Helper function to parse a single binding line (no 'let' keyword)
parseSingleBindingLine :: Parser (String, Expr)
parseSingleBindingLine = do
  traceM "Parsing single binding line (identifier = expression)"
  checkIndent GreaterEq
  nameTok <- matchToken isValidName "identifier"

  -- Optional type annotation
  annotatedType <- do
    st <- get
    case stateTokens st of
      (tok : _) | locToken tok == TColon -> do
        traceM "Found type annotation"
        _ <- matchToken (== TColon) ":"
        typeTok <- matchToken isValidName "type name"
        declaredType <- parseTypeToken typeTok
        return $ Just declaredType
      _ -> return Nothing

  _ <- matchToken (== (TOperator "=")) "equals sign"
  traceM "Parsing value for binding line"
  value <- parseExpressionWithType annotatedType

  -- If we have a type annotation, ensure value matches it
  case annotatedType of
    Just (TypeNum expectedType) ->
      case value of
        Lit (IntLit val mtype) ->
          -- Add support for new style int literals
          case mtype of
            Just actualType ->
              if actualType == expectedType
                then return ()
                else
                  throwError $
                    UnexpectedToken
                      (Located (TNumber val) 0 0)
                      ("integer literal of type " ++ show expectedType)
            Nothing -> return () -- Inferred type will be handled by convertToIR
        Lit (FloatLit val mtype) ->
          -- Add support for new style float literals
          case mtype of
            Just actualType ->
              if actualType == expectedType
                then return ()
                else
                  throwError $
                    UnexpectedToken
                      (Located (TNumber val) 0 0)
                      ("float literal of type " ++ show expectedType)
            Nothing -> return () -- Inferred type will be handled by convertToIR
        _ -> return () -- Allow other expressions for now
    _ -> return ()

  case locToken nameTok of
    TWord varName -> do
      -- Register annotated type if present
      case annotatedType of
        Just typ -> do
          modify $ \s ->
            s
              { stateSymTable =
                  registerVarType varName typ (stateSymTable s)
              }
        Nothing -> return ()

      -- Register type if we can determine it
      case value of
        Call structName' _ -> do
          -- Struct instantiation
          st <- get
          case M.lookup structName' (structNames $ stateSymTable st) of
            Just sid ->
              modify $ \s ->
                s
                  { stateSymTable =
                      registerVarType varName (TypeStruct sid structName') (stateSymTable s)
                  }
            Nothing -> return ()
        Let _ _ -> return () -- Let expressions
        Block _ _ _ -> return () -- Block expressions
        Break _ _ -> return () -- Break statements
        Result _ -> return () -- Result expressions
        BinOp _ _ _ -> return () -- Binary operations
        If _ _ _ -> return () -- If expressions
        StructLit _ _ -> return () -- Raw struct literals
        FieldAccess _ _ -> return () -- Field access
        ArrayLit _ _ -> return () -- Array literals
        Index _ _ -> return () -- Array indexing
        While _ _ -> return () -- While loops
        VarDecl _ _ -> return () -- Variable declarations
        Assign _ _ -> return () -- Assignments
        AssignOp _ _ _ -> return () -- Compound assignments
        Lit _ -> return () -- Literals
        Var _ -> return () -- Variable references
      return (varName, value)
    _ -> throwError $ UnexpectedToken nameTok "identifier"

parseBlockImpl :: Parser Expr
parseBlockImpl = do
  st <- get

  _ <- matchToken isBlockKeyword "block"
  nameTok <- matchToken isValidName "block name"
  _ <- matchToken isColon ":"

  let blockName = case locToken nameTok of
        TWord name -> name
        _ -> error "Invalid block name"

  let currentIndent = stateIndent st
  let newIndent = currentIndent + 2
  let ctx = makeIndentContext BasicBlock newIndent currentIndent

  -- Set new indent context
  modify $ \s -> s {stateIndent = newIndent}

  -- Parse block contents with context
  (exprs, resultExpr) <- parseBlockContents ctx

  return $ Block blockName exprs resultExpr

-- Helper for block contents
parseBlockContents :: IndentContext -> Parser ([Expr], Maybe Expr)
parseBlockContents ctx = do
  modify $ \s -> s {stateIndent = baseIndent ctx}

  st <- get
  traceM $ "\n=== parseBlockContents ==="
  traceM $ "Context type: " ++ show ctx
  traceM $ "Current indent: " ++ show (stateIndent st)
  traceM $ "Current tokens: " ++ show (take 3 $ stateTokens st)

  case stateTokens st of
    [] -> return ([], Nothing)
    (tok : rest) -> case locToken tok of
      TEOF -> return ([], Nothing)
      _ -> do
        traceM $ "Block contents token: " ++ show tok
        traceM $ "Current indent context: base=" ++ show (baseIndent ctx)
        traceM $ "Current state indent: " ++ show (stateIndent st)
        traceM $ "Token column: " ++ show (locCol tok)
        traceM $ "Token line: " ++ show (locLine tok)

        -- Add line gap detection
        let nextLineNum = case rest of
              (next : _) -> locLine next
              [] -> locLine tok
        let hasLineGap = nextLineNum > locLine tok + 1

        traceM $ "Next line: " ++ show nextLineNum
        traceM $ "Line gap detected: " ++ show hasLineGap

        -- Regular indentation check
        when (isBlockStatement (locToken tok) && locCol tok < baseIndent ctx) $
          throwError $
            IndentationError $
              IndentationErrorDetails (baseIndent ctx) (locCol tok) GreaterEq

        let shouldContinue = locCol tok >= baseIndent ctx
        if shouldContinue
          then do
            -- Parse current expression
            expr <- case locToken tok of
              TWord "print" -> parsePrintStatement
              TWord "block" -> parseBlockImpl
              _ -> parseExpression

            -- Check if next token is dedented or after gap
            case rest of
              (next : _)
                | hasLineGap && locCol next < baseIndent ctx ->
                    -- This expr is last one - make it the result
                    return ([], Just expr)
                | locCol next < baseIndent ctx ->
                    -- This expr is last one - make it the result
                    return ([], Just expr)
                | otherwise -> do
                    -- More expressions follow
                    (rest', result) <- parseBlockContents ctx
                    return (expr : rest', result)
              [] ->
                -- Last expression becomes the result
                return ([], Just expr)
          else return ([], Nothing)

-- Helper to identify statements that must be properly indented in blocks
isBlockStatement :: Token -> Bool
isBlockStatement (TWord "print") = True
isBlockStatement (TWord "var") = True
isBlockStatement (TWord "let") = True
isBlockStatement _ = False

parseBlockExprs :: Maybe Type -> BlockType -> Int -> Parser ([Expr], Maybe Expr)
parseBlockExprs expectedType bt bi = do
  st <- get
  traceM $ "\n=== parseBlockExprs ==="
  traceM $ "Block type: " ++ show bt
  traceM $ "Initial tokens: " ++ show (take 3 $ stateTokens st)
  traceM $ "Base indent: " ++ show bi
  traceM $ "Current indent: " ++ show (stateIndent st)

  case stateTokens st of
    [] -> return ([], Nothing)
    (tok : _) -> do
      let tokCol = locCol tok
      traceM $ "Next token col: " ++ show tokCol
      traceM $ "Next token: " ++ show (locToken tok)

      -- Only check indentation if we're not at a block header
      let shouldCheckIndent = case locToken tok of
            TWord "block" -> False
            TWord "fn" -> False
            _ -> True

      when shouldCheckIndent $ do
        traceM $ "Checking indentation: token col " ++ show tokCol ++ " against base " ++ show bi
        checkBlockIndent bt bi

      if locToken tok == TEOF
        then return ([], Nothing)
        else
          if tokCol < bi && shouldCheckIndent
            then do
              traceM $ "Token column " ++ show tokCol ++ " < base indent " ++ show bi
              return ([], Nothing)
            else case locToken tok of
              TWord "else" -> do
                -- Stop parsing block when we hit else
                traceM "Found else token, ending block"
                return ([], Nothing)
              TWord "print" -> do
                traceM "Found print statement in block - handling specially"
                printExpr <- parsePrintStatement
                traceM $ "Parsed print statement: " ++ show printExpr
                (moreExprs, resultExpr) <- parseBlockExprs expectedType bt bi
                return (printExpr : moreExprs, resultExpr)
              TWord "while" -> do
                traceM "Found while statement in block"
                currentIndent <- gets stateIndent -- Save current indent
                whileExpr <- parseWhileExpr expectedType
                modify $ \s -> s {stateIndent = currentIndent} -- Restore indent
                (moreExprs, resultExpr) <- parseBlockExprs expectedType bt bi
                return (whileExpr : moreExprs, resultExpr)
              TWord "result" -> do
                traceM "Parsing result expression"
                resultExpr <- parseResultImpl
                breaks <- parsePostResult bi
                traceM $ "Result parsed with " ++ show (length breaks) ++ " following breaks"
                return (breaks, Just resultExpr)
              TWord "break" -> do
                traceM "Parsing break statement"
                breakExpr <- parseBreakImpl
                (moreExprs, resultExpr) <- parseBlockExprs expectedType BasicBlock bi
                return (breakExpr : moreExprs, resultExpr)
              TWord "var" -> do
                traceM "Parsing variable declaration"
                varExpr <- parseVarDecl
                (moreExprs, resultExpr) <- parseBlockExprs expectedType BasicBlock bi
                return (varExpr : moreExprs, resultExpr)
              TWord "block" -> do
                traceM "Parsing block without wrapping"
                expr <- parseBlockImpl -- Direct parse
                return ([expr], Nothing) -- Don't wrap in an extra block
              TWord _ | isValidName (locToken tok) -> do
                -- Look ahead for assignment
                st' <- get
                case drop 1 $ stateTokens st' of
                  (t : _) | locToken t == TOperator "=" -> do
                    assignExpr <- parseAssign expectedType
                    (moreExprs, resultExpr) <- parseBlockExprs expectedType BasicBlock bi
                    return (assignExpr : moreExprs, resultExpr)
                  _ -> do
                    expr <- parseExpressionWithType expectedType
                    (moreExprs, resultExpr) <- parseBlockExprs expectedType BasicBlock bi
                    return (expr : moreExprs, resultExpr)
              _ -> do
                traceM "Parsing block expression"
                expr <- parseExpressionWithType expectedType -- parse this one line

                -- Decide if this expression becomes the final "block result" or not.
                st2 <- get
                case stateTokens st2 of
                  (nextTok : _) -> do
                    let col2 = locCol nextTok
                    let lineGap = locLine nextTok > locLine tok + 1

                    -- We'll treat expr as the final result ONLY if the block is ending
                    -- (dedent or line gap) AND the expr is not an if/break line.
                    let isIfOrBreak e = case e of
                          If {} -> True
                          Break {} -> True
                          _ -> False

                    if (col2 < bi || lineGap) && not (isIfOrBreak expr)
                      then do
                        traceM "Block ends here (dedent or line gap). This expr is the final result."
                        return ([], Just expr)
                      else do
                        -- Otherwise, parse more lines in the same block
                        (restStmts, restResult) <- parseBlockExprs expectedType bt bi
                        return (expr : restStmts, restResult)
                  [] -> do
                    -- No more tokens => single line is final result
                    traceM "No more tokens. Single-line block => final result."
                    return ([], Just expr)

-- parseBlockExprs :: Maybe Type -> BlockType -> Int -> Parser ([Expr], Maybe Expr)
-- parseBlockExprs expectedType bt bi = do
--     st <- get
--     traceM $ "\n=== parseBlockExprs ==="
--     traceM $ "Block type: " ++ show bt
--     traceM $ "Initial tokens: " ++ show (take 3 $ stateTokens st)
--     traceM $ "Base indent: " ++ show bi
--     traceM $ "Current indent: " ++ show (stateIndent st)

--     case stateTokens st of
--         [] -> return ([], Nothing)
--         (tok:_) -> do
--             let tokCol = locCol tok
--             traceM $ "Next token col: " ++ show tokCol
--             traceM $ "Next token: " ++ show (locToken tok)

--             -- Only check indentation if we're not at a block header
--             let shouldCheckIndent = case locToken tok of
--                   TWord "block" -> False
--                   TWord "fn" -> False
--                   _ -> True

--             when shouldCheckIndent $ do
--                 traceM $ "Checking indentation: token col " ++ show tokCol ++ " against base " ++ show bi
--                 checkBlockIndent bt bi

--             if locToken tok == TEOF
--                 then return ([], Nothing)
--                 else if tokCol < bi && shouldCheckIndent
--                     then do
--                         traceM $ "Token column " ++ show tokCol ++ " < base indent " ++ show bi
--                         return ([], Nothing)
--                     else case locToken tok of
--                         TWord "else" -> do   -- Stop parsing block when we hit else
--                             traceM "Found else token, ending block"
--                             -- return ([Lit (IntLit "1" (Just Int64))], Nothing)
--                             return ([], Nothing)
--                         TWord "print" -> do
--                             traceM "Found print statement in block - handling specially"
--                             printExpr <- parsePrintStatement
--                             traceM $ "Parsed print statement: " ++ show printExpr
--                             (moreExprs, resultExpr) <- parseBlockExprs expectedType bt bi
--                             return (printExpr : moreExprs, resultExpr)

--                         TWord "while" -> do
--                             traceM "Found while statement in block"
--                             currentIndent <- gets stateIndent  -- Save current indent
--                             whileExpr <- parseWhileExpr expectedType
--                             modify $ \s -> s { stateIndent = currentIndent }  -- Restore indent
--                             (moreExprs, resultExpr) <- parseBlockExprs expectedType bt bi
--                             return (whileExpr : moreExprs, resultExpr)

--                         TWord "result" -> do
--                             traceM "Parsing result expression"
--                             resultExpr <- parseResultImpl
--                             breaks <- parsePostResult bi
--                             traceM $ "Result parsed with " ++ show (length breaks) ++ " following breaks"
--                             return (breaks, Just resultExpr)

--                         TWord "break" -> do
--                             traceM "Parsing break statement"
--                             breakExpr <- parseBreakImpl
--                             (moreExprs, resultExpr) <- parseBlockExprs expectedType BasicBlock bi
--                             return (breakExpr : moreExprs, resultExpr)

--                         TWord "var" -> do
--                             traceM "Parsing variable declaration"
--                             varExpr <- parseVarDecl
--                             (moreExprs, resultExpr) <- parseBlockExprs expectedType BasicBlock bi
--                             return (varExpr : moreExprs, resultExpr)

--                         TWord "block" -> do
--                             traceM "Parsing block without wrapping"
--                             expr <- parseBlockImpl  -- Direct parse
--                             return ([expr], Nothing)  -- Don't wrap

--                         TWord _ | isValidName (locToken tok) -> do
--                             -- Look ahead for assignment
--                             st' <- get
--                             case drop 1 $ stateTokens st' of
--                                 (t:_) | locToken t == TOperator "=" -> do
--                                     assignExpr <- parseAssign expectedType
--                                     (moreExprs, resultExpr) <- parseBlockExprs expectedType BasicBlock bi
--                                     return (assignExpr : moreExprs, resultExpr)
--                                 _ -> do
--                                     expr <- parseExpressionWithType expectedType
--                                     (moreExprs, resultExpr) <- parseBlockExprs expectedType BasicBlock bi
--                                     return (expr : moreExprs, resultExpr)

--                         _ -> do
--                           traceM "Parsing block expression"
--                           expr <- parseExpressionWithType expectedType  -- parse this one line

--                           -- Look at the next token to see if the block ends
--                           st2 <- get
--                           case stateTokens st2 of
--                             (nextTok:_) ->
--                               -- If next line is dedented or there's a line gap => make `expr` the final result
--                               if locCol nextTok < bi
--                                  then do
--                                    traceM "Block ends here (dedent or line gap). This expr is the final result."
--                                    return ([], Just expr)
--                                  else do
--                                    -- Otherwise, parse more lines in the same block
--                                    (restStmts, restResult) <- parseBlockExprs expectedType bt bi
--                                    return (expr : restStmts, restResult)
--                             [] -> do
--                               -- No more tokens => single line is final result
--                               traceM "No more tokens. Single-line block => final result."
--                               return ([], Just expr)
--                         -- _ -> do
--                         --     traceM "Parsing block expression"
--                         --     expr <- parseExpressionWithType expectedType
--                         --     (moreExprs, resultExpr) <- parseBlockExprs expectedType BasicBlock bi
--                         --     return (expr : moreExprs, resultExpr)

parseBreakImpl :: Parser Expr
parseBreakImpl = do
  traceM $ "\n=== parseFuncDecl ==="
  checkIndent GreaterEq
  _ <- matchToken isBreak "break"

  -- Parse optional label
  label <- do
    st <- get
    traceM $ "Parsing label at indent: " ++ show (stateIndent st)
    case stateTokens st of
      (tok : _) | isValidName (locToken tok) -> do
        labelTok <- matchToken isValidName "block label"
        case locToken labelTok of
          TWord l -> do
            traceM $ "Parsed label: " ++ show l
            return $ Just l
          _ -> error "Matched non-word token as break label"
      _ -> return Nothing

  -- Parse optional value after label
  value <- case label of
    Just _ -> do
      st <- get
      traceM $ "Parsing value indent: " ++ show (stateIndent st)
      case stateTokens st of
        (tok : _) | isValidName (locToken tok) -> do
          expr <- parseExpression
          traceM $ "Parsed value: " ++ show expr
          return $ Just expr
        _ -> return Nothing
    Nothing -> return Nothing

  return $ Break label value

parseResultImpl :: Parser Expr
parseResultImpl = do
  st <- get
  traceM $ "parseResult at indent " ++ show (stateIndent st)
  checkIndent GreaterEq
  _ <- matchToken isResult "result"
  traceM "Parsing result expression value"
  expr <- parseBasicExprImpl
  return $ Result expr

parsePostResult :: Int -> Parser [Expr]
parsePostResult bi = do
  st <- get
  traceM $ "parsePostResult at indent " ++ show bi
  case stateTokens st of
    [] -> return []
    (tok : _) -> do
      traceM $ "Post-result token: " ++ show tok
      if locToken tok == TEOF
        then return []
        else do
          when (locCol tok < bi) $
            throwError $
              IndentationError $
                IndentationErrorDetails bi (locCol tok) GreaterEq
          case locToken tok of
            TWord "break" -> do
              traceM "Parsing post-result break statement"
              breakExpr <- parseBreakImpl
              rest <- parsePostResult bi
              return (breakExpr : rest)
            TWord "result" ->
              throwError $ UnexpectedToken tok "only break statements allowed after result"
            _ -> throwError $ UnexpectedToken tok "only break statements allowed after result"

parseVectorLiteral :: Parser Expr
parseVectorLiteral = do
  constructorTok <- matchToken isVecConstructor "vector constructor"
  case locToken constructorTok of
    TVec vecTypeStr -> do
      traceM $ "Parsing vector constructor: " ++ show vecTypeStr
      -- After reading the constructor, parse as a call
      parseMaybeCall vecTypeStr
    _ -> throwError $ UnexpectedToken constructorTok "vector constructor"

parsePrintStatement :: Parser Expr
parsePrintStatement = do
  traceM "Parsing print statement"
  expectedIndent <- gets stateIndent
  st <- get

  case stateTokens st of
    (tok : _) -> do
      when (locCol tok < expectedIndent) $
        throwError $
          IndentationError $
            IndentationErrorDetails expectedIndent (locCol tok) GreaterEq
      _ <- matchToken isPrint "print"

      -- Parse the argument as a complete expression
      -- This change ensures we parse the entire expression including parentheses
      arg <- parseExpression
      traceM $ "Parsed print argument: " ++ show arg
      return $ Call "print" [arg]
    [] -> throwError $ EndOfInput "print"

parseWhileExpr :: Maybe Type -> Parser Expr
parseWhileExpr expectedType = do
  traceM "Parsing while expression"
  _ <- matchToken (\t -> t == TWord "while") "while keyword"
  condition <- parseExpression
  _ <- matchToken isColon ":"
  bodyState <- get
  traceM $ "While loop body state: " ++ show bodyState

  -- Get current parent block's base indent
  let parentIndent' = stateIndent bodyState
  traceM $ "Parent indent level: " ++ show parentIndent'

  -- Calculate child block indent relative to parent
  let blockIndent' = parentIndent' + 2
  traceM $ "Setting block indent to: " ++ show blockIndent'

  -- Parse block contents at new indent level
  let savedIndent = stateIndent bodyState
  modify $ \s -> s {stateIndent = blockIndent'}
  (bodyExprs, mResult) <- parseBlockExprs expectedType BasicBlock blockIndent'
  modify $ \s -> s {stateIndent = savedIndent} -- Restore parent indent
  let body = Block "while_body" bodyExprs mResult
  traceM $ "Parsed while body: " ++ show body
  return $ While condition body

parseFuncDecl :: Parser Decl
parseFuncDecl = do
  traceM "\n=== parseFuncDecl ==="
  oldIndent <- gets stateIndent
  traceM $ "Starting indent: " ++ show oldIndent

  traceM "Parsing function declaration"
  _ <- matchToken (\t -> t == TWord "fn") "fn"
  nameTok <- matchToken isValidName "function name"
  let funcName' = case locToken nameTok of
        TWord name -> name
        _ -> error "Expected function name"

  typeParams <- parseTypeParams
  traceM $ "Parsed type parameters: " ++ show typeParams

  _ <- matchToken (== TLeftParen) "("

  params <- parseParams

  _ <- matchToken (== TColon) ":"
  retTypeTok <- matchToken isValidName "return type"
  retType <- parseTypeToken retTypeTok

  _ <- matchToken (== (TOperator "=")) "="

  -- Create function context with standard indent
  let indent = 2
  let ctx = makeIndentContext FunctionBlock indent 0

  -- Parse first expression with indentation
  traceM "=== Parsing function body ==="
  st <- get

  case stateTokens st of
    (tok : _) -> do
      when (locCol tok < indent) $
        throwError $
          IndentationError $
            IndentationErrorDetails indent (locCol tok) GreaterEq

      expr <- parseExpression

      -- Check next token for line gap and dedent
      st' <- get
      case stateTokens st' of
        (next : _)
          | locLine next > locLine tok + 1 && locCol next < indent -> do
              traceM $ "Found line gap and dedent - ending function body"
              modify $ \s -> s {stateIndent = 0} -- Reset indent for top level
              let body = Block funcName' [expr] Nothing -- Changed from "function_body"
              case locToken nameTok of
                TWord name -> return $ DFunc name typeParams params retType body
                _ -> throwError $ UnexpectedToken nameTok "function name"
          | otherwise -> do
              -- Continue parsing block contents normally
              (rest, result) <- parseBlockContents ctx
              modify $ \s -> s {stateIndent = 0} -- Reset indent for top level
              let body = Block funcName' (expr : rest) result -- Changed from "function_body"
              case locToken nameTok of
                TWord name -> return $ DFunc name typeParams params retType body
                _ -> throwError $ UnexpectedToken nameTok "function name"
        [] -> throwError $ EndOfInput "function body"
    [] -> throwError $ EndOfInput "function body"

-- Helper to parse parameters with shared type annotation
parseParams :: Parser [Param]
parseParams = do
  traceM "Parsing function parameters"
  -- Parse comma-separated parameter names
  names <- parseParamNames
  traceM $ "Parsed parameter names: " ++ show names

  -- Parse shared type annotation
  _ <- matchToken (== TColon) ":"
  typeTok <- matchToken isValidName "parameter type"
  paramType <- parseTypeToken typeTok
  traceM $ "Parsed parameter type: " ++ show paramType

  _ <- matchToken (== TRightParen) ")"

  -- Create params with shared type
  return [Param name paramType | name <- names]
  where
    parseParamNames :: Parser [String]
    parseParamNames = do
      nameTok <- matchToken isValidName "parameter name"
      case locToken nameTok of
        TWord name -> do
          st <- get
          case stateTokens st of
            (tok : _) | locToken tok == TComma -> do
              _ <- matchToken (== TComma) ","
              rest <- parseParamNames
              return (name : rest)
            _ -> return [name]
        _ -> throwError $ UnexpectedToken nameTok "parameter name"

-- Helper to parse type tokens
parseTypeToken :: Located -> Parser Type
parseTypeToken tok = case locToken tok of
  TWord "i32" -> return $ TypeNum Int32
  TWord "i64" -> return $ TypeNum Int64
  TWord "f32" -> return $ TypeNum Float32
  TWord "f64" -> return $ TypeNum Float64
  TWord (c : _) | isUpper c -> return $ TypeParam [c]
  TWord _ -> throwError $ UnexpectedToken tok "valid type"
  _ -> throwError $ UnexpectedToken tok "valid type"

parseVarDecl :: Parser Expr
parseVarDecl = do
  traceM "Parsing variable declaration"
  _ <- matchToken (\t -> t == TWord "var") "var"
  nameTok <- matchToken isValidName "identifier"

  -- Parse optional type annotation
  annotatedType <- do
    st <- get
    case stateTokens st of
      (tok : _) | locToken tok == TColon -> do
        traceM "Found type annotation"
        _ <- matchToken (== TColon) ":"
        typeTok <- matchToken isValidName "type name"
        declaredType <- parseTypeToken typeTok
        return $ Just declaredType
      _ -> return Nothing

  _ <- matchToken (== (TOperator "=")) "equals sign"
  value <- parseExpression

  case locToken nameTok of
    TWord name -> do
      -- Register variable type if annotation was present
      case annotatedType of
        Just typ -> do
          modify $ \s ->
            s
              { stateSymTable =
                  registerVarType name typ (stateSymTable s)
              }
        Nothing -> return ()

      return $ VarDecl name value
    _ -> throwError $ UnexpectedToken nameTok "identifier"

parseExprFromText :: T.Text -> Either ParseError Expr
parseExprFromText input = do
  tokens <- mapLexError $ tokenize input
  runParser parseExpression tokens

defaultExprParser :: ExprParser
defaultExprParser =
  ExprParser
    { parseBasicExpr = parseBasicExprImpl,
      parseBlock = parseBlockImpl,
      parseBreak = parseBreakImpl,
      parseResult = parseResultImpl
    }

mapLexError :: Either LexError a -> Either ParseError a
mapLexError (Left (UnterminatedString line col)) =
  Left $ EndOfInput $ "Unterminated string at line " ++ show line ++ ", column " ++ show col
mapLexError (Left (InvalidCharacter c line col)) =
  Left $ EndOfInput $ "Invalid character '" ++ [c] ++ "' at line " ++ show line ++ ", column " ++ show col
mapLexError (Right a) = Right a
