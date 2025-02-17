{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Zap.Parser.Program
  ( parseProgram,
    parseTopLevel,
    parseStructFields,
  )
where

import Control.Monad (when)
import Control.Monad.Except
import Control.Monad.State
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Debug.Trace
import Zap.AST
import Zap.Analysis.Lexical
import Zap.Parser.Core
import Zap.Parser.Expr
import Zap.Parser.Types

parseProgram :: T.Text -> Either ParseError ([TopLevel], SymbolTable)
parseProgram input = do
  traceM "\n=== Starting Program Parsing ==="
  traceM $ "Input text: " ++ T.unpack input
  tokens <- mapLexError $ tokenize input
  traceM $ "Tokenized input: " ++ show tokens

  -- 1) Parse into a list of TopLevel plus final parser state (which holds the SymbolTable).
  (tops, finalState) <- runParserWithState parseTopLevelsWithRegistration tokens
  traceM $ "Parsed top-levels: " ++ show tops

  let symTable = stateSymTable finalState
      -- Wrap the raw [TopLevel] into a Program AST
      rawProgram = Program tops
  traceM $ "Initial symbol table after registration: " ++ show symTable

  -- 2) Use your new pass to fix TypeUnresolved placeholders to TypeStruct if possible
  let resolvedProgram = resolveUnresolvedTypes rawProgram symTable
  let symTableFixed = fixSymbolTableTypes symTable
  traceM $ "Symbol table after specialization: " ++ show symTableFixed
  traceM $ "Resolved program: " ++ show resolvedProgram

  -- 3) Unwrap the final Program back into [TopLevel] for your return value
  let finalTops = case resolvedProgram of
        Program ts -> ts

  traceM $ "Final top-levels: " ++ show finalTops

  return (finalTops, symTableFixed)

runParserWithState :: Parser a -> [Located] -> Either ParseError (a, ParseState)
runParserWithState p tokens = runStateT p (ParseState tokens 0 0 emptySymbolTable tokens)

parseTopLevelsWithRegistration :: Parser [TopLevel]
parseTopLevelsWithRegistration = do
  -- First pass: Register all structs
  st <- get
  let originalTokens = stateTokens st
  registerStructs
  modify $ \s -> s {stateTokens = originalTokens}

  -- Second pass: Parse all top-level expressions
  parseTopLevels

-- | Perform the initial pass to register all structs
registerStructs :: Parser ()
registerStructs = do
  st <- get
  traceM $ "\n=== Starting registerStructs ==="
  case stateTokens st of
    [] -> do
      traceM "No more tokens to process. Exiting registerStructs."
      return () -- Base case: no more tokens
    (tok : _) ->
      case locToken tok of
        TWord "type" -> do
          traceM $ "Found type block, parsing type declarations..."
          -- Parse the type declaration using parseSingleTypeDecl
          topLevel <- parseSingleTypeDecl
          case topLevel of
            TLType name (TypeStruct sid _) -> do
              traceM $ "Found declaration for struct with id: " ++ show sid ++ " and name: " ++ show name
              -- Register the struct
              st <- get
              let (sid, newSt) = registerParamStruct name [] [] (stateSymTable st)
              modify $ \s -> s {stateSymTable = newSt}
              traceM $ "Registered struct: " ++ name ++ " with SID: " ++ show sid
              -- Continue with the rest of the tokens
              registerStructs
            d -> do
              -- Ignore non-struct type declarations
              traceM $ "Found non-struct type declaration. Ignoring."
              registerStructs
        t -> do
          traceM $ "Continuing with struct registration. Processing token: " ++ show t
          modify $ \s -> s {stateTokens = rest} -- Consume the token
          registerStructs -- Continue with the rest of the tokens
      where
        rest = drop 1 $ stateTokens st

parseTopLevels :: Parser [TopLevel]
parseTopLevels = do
  st <- get
  traceM $ "\n=== Starting parseTopLevels ==="
  traceM $ "Current state indent: " ++ show (stateIndent st)
  traceM $ "Current tokens: " ++ show (take 5 $ stateTokens st)
  traceM $ "Current symTable: " ++ show (stateSymTable st)

  let ptlLoop ptlAcc =
        let loop = do
              st' <- get
              traceM $ "\n=== Top-level parsing loop ==="
              traceM $ "Current accumulator: " ++ show ptlAcc
              case stateTokens st' of
                [] -> return $ reverse ptlAcc
                (tok : _) -> case locToken tok of
                  TEOF -> do
                    traceM "Encountered TEOF while parsing top level expressions"
                    return $ reverse ptlAcc
                  TType -> do
                    traceM "Encountered TType while parsing top level expressions"
                    origIndent <- gets stateIndent
                    -- Consume `type` keyword
                    _ <- matchToken (== TType) "type"

                    -- First pass: register all structs in this type block
                    let registerTypes = do
                          st'' <- get
                          case stateTokens st'' of
                            (regTok : _)
                              | isTypeKeyword (locToken regTok) -> do
                                  -- Found next type declaration at same level
                                  traceM $ "\n=== Pre-registering type at base indent ==="
                                  nameTok <- matchToken isValidName "type name"
                                  params <- parseTypeParams
                                  _ <- matchToken (== (TOperator "=")) "equals"
                                  _ <- matchToken (== TStruct) "struct"

                                  let name = case locToken nameTok of
                                        TWord n -> n
                                        _ -> error "Expected name token"

                                  traceM $ "Pre-registering struct: " ++ name
                                  traceM $ "Type parameters: " ++ show params

                                  let (sid, newSt) = registerParamStruct name params [] (stateSymTable st'')
                                  modify $ \s -> s {stateSymTable = newSt}

                                  traceM $ "Registered " ++ name ++ " with SID: " ++ show sid
                                  traceM $ "Updated symbol table: " ++ show newSt

                                  -- Skip struct fields to next potential type declaration
                                  skipStructBody
                                  registerTypes
                              | otherwise -> do
                                  traceM "Finished pre-registering types"
                                  return ()
                            _ -> return ()

                    -- Do registration pass
                    registerTypes

                    -- Reset position to start of type block for parsing
                    modify $ \s -> s {stateTokens = drop 1 $ stateTokens st'}

                    -- Now parse type declarations with registered names
                    let typeLoop typeAcc =
                          let innerLoop = do
                                st'' <- get
                                traceM $ "\n=== Type block parsing loop ==="
                                traceM $ "Type accumulator: " ++ show typeAcc
                                case stateTokens st'' of
                                  (tok' : _)
                                    | locToken tok' == TEOF -> do
                                        traceM "Encountered TEOF in type block parsing loop!"
                                        modify $ \s -> s {stateIndent = origIndent}
                                        let finalAcc = typeAcc ++ ptlAcc
                                        traceM $ "Final accumulator after TEOF: " ++ show finalAcc
                                        return $ reverse finalAcc
                                    | locCol tok' > locCol tok -> do
                                        traceM $ "Parsing type declaration at col " ++ show (locCol tok')
                                        modify $ \s -> s {stateIndent = locCol tok'}
                                        td <- parseSingleTypeDecl
                                        traceM $ "Parsed type declaration: " ++ show td
                                        typeLoop (td : typeAcc)
                                    | locCol tok' <= locCol tok -> do
                                        traceM $
                                          "Found token at col "
                                            ++ show (locCol tok')
                                            ++ " <= base indent "
                                            ++ show (locCol tok)
                                            ++ ", ending type block"
                                        modify $ \s -> s {stateIndent = origIndent}
                                        let newAcc = typeAcc ++ ptlAcc
                                        traceM $ "Continuing with accumulator: " ++ show newAcc
                                        ptlLoop newAcc -- Continue with accumulated declarations
                                  _ -> do
                                    traceM "No more tokens in type block!"
                                    modify $ \s -> s {stateIndent = origIndent}
                                    let finalAcc = typeAcc ++ ptlAcc
                                    traceM $ "Final accumulator at end: " ++ show finalAcc
                                    return $ reverse finalAcc
                           in innerLoop

                    typeResult <- typeLoop []
                    traceM $ "All type declarations parsed in type block: " ++ show typeResult
                    return typeResult
                  _ -> do
                    traceM $ "Encountered token while parsing top level expressions: " ++ show tok
                    expr <- parseTopLevel
                    traceM $ "Parsed expression: " ++ show expr
                    ptlLoop (expr : ptlAcc)
         in loop

  ptlLoop []
  where
    isTypeKeyword :: Token -> Bool
    isTypeKeyword (TWord "type") = True
    isTypeKeyword _ = False

-- Helper to skip to next top-level declaration
-- Helper function to skip the body of a struct definition
skipStructBody :: Parser ()
skipStructBody = do
  st <- get
  let baseIndent = stateIndent st
  case stateTokens st of
    [] -> return () -- Base case: no more tokens
    (tok : rest) -> do
      if locCol tok > baseIndent
        then do
          modify $ \s -> s {stateTokens = rest} -- Consume the token
          skipStructBody -- Continue with the rest of the tokens
        else return () -- Stop when indentation is no longer greater

parseTopLevel :: Parser TopLevel
parseTopLevel = do
  st <- get
  traceM "\n--- Parsing Single Top Level Expression ---"
  traceM $ "Current state indent: " ++ show (stateIndent st)
  traceM $ "Current state: " ++ show st
  -- Store original indent level
  origIndent <- gets stateIndent
  case stateTokens st of
    (tok : _) -> do
      traceM $ "Processing token: " ++ show tok
      traceM $ "Token column: " ++ show (locCol tok)
      case locToken tok of
        TWord "while" -> do
          traceM "Found while expression at top-level"
          expr <- parseWhileExpr Nothing
          return $ TLExpr expr
        TWord "var" -> do
          traceM "Found variable declaration at top-level"
          expr <- parseVarDecl
          return $ TLExpr expr
        TWord "print" -> do
          traceM "Found print statement at top-level"
          if origIndent > 0
            then do
              traceM $ "Checking print indentation against level: " ++ show origIndent
              when (locCol tok < origIndent) $
                throwError $
                  IndentationError $
                    IndentationErrorDetails origIndent (locCol tok) GreaterEq
              expr <- parsePrintStatement
              return $ TLExpr expr
            else do
              expr <- parsePrintStatement
              return $ TLExpr expr
        TWord "block" -> do
          traceM "Found block at top level - parsing directly"
          expr <- parseBlockImpl -- Parse block directly
          return $ TLExpr expr -- Don't wrap in extra block
        TWord "let" -> do
          traceM "Found let block at top-level"
          expr <- parseLetBlock
          traceM $ "Parsed let block: " ++ show expr
          return $ TLExpr expr
        TWord "fn" -> do
          traceM "Found function declaration at top-level"
          -- Save current indent before parsing function
          modify $ \s -> s {stateIndent = 0}
          -- Parse the full function declaration
          decl <- parseFuncDecl
          -- Restore indent after function
          modify $ \s -> s {stateIndent = origIndent}
          traceM $ "Parsed function declaration: " ++ show decl
          return $ TLDecl decl
        TWord name | isValidName (locToken tok) -> do
          traceM $ "Found identifier: " ++ name
          traceM $ "Token indent: " ++ show (locCol tok)
          traceM $ "State indent: " ++ show (stateIndent st)
          traceM $ "Parsing expression"
          expr <- parseExpression
          traceM $ "Parsed expression: " ++ show expr
          return $ TLExpr expr
        _ -> do
          traceM $ "Unexpected token in top level: " ++ show tok
          throwError $ UnexpectedToken tok "expected 'print', 'let', or 'block'"
    [] -> do
      traceM "No tokens available for top-level expression"
      throwError $ EndOfInput "expected top-level expression"

parseLetBlock :: Parser Expr
parseLetBlock = do
  traceM "Parsing let binding"
  _ <- matchToken (\t -> t == TWord "let") "let keyword"

  -- Parse first binding
  (varName, value) <- parseSingleBindingLine
  traceM $ "First binding parsed: Let " ++ show varName ++ " " ++ show value

  -- Check for more bindings
  moreBindings <- parseMoreBindings 2
  traceM $ "Additional bindings parsed: " ++ show moreBindings

  case moreBindings of
    [] -> do
      traceM "Single let binding - returning direct Let expression"
      return $ Let varName value
    _ -> do
      traceM "Multiple bindings - wrapping in block"
      let allBindings = Let varName value : map (\(n, v) -> Let n v) moreBindings
      return $ Block "top_let" allBindings Nothing
  where
    parseMoreBindings :: Int -> Parser [(String, Expr)]
    parseMoreBindings indent = do
      st <- get
      traceM $ "Checking for more bindings at indent " ++ show indent ++ ": " ++ show (take 3 $ stateTokens st)
      case stateTokens st of
        [] -> return []
        (tok : _)
          | locToken tok == TEOF -> return []
          | locCol tok < indent -> do
              -- Only end if we actually dedent
              traceM $ "Let block ended due to dedent: " ++ show (locCol tok) ++ " < " ++ show indent
              return []
          | locToken tok == TWord "print" && locCol tok == 1 -> do
              -- End block when we see a non-indented print
              traceM "Let block ended due to top-level print"
              return []
          | locCol tok == indent || locCol tok > indent -> do
              -- Continue parsing bindings at same or greater indentation
              if isValidName (locToken tok)
                then do
                  traceM "Found another binding line"
                  (n, v) <- parseSingleBindingLine
                  rest <- parseMoreBindings indent
                  return ((n, v) : rest)
                else do
                  traceM $ "No more bindings (token not a name): " ++ show tok
                  return []
          | otherwise -> do
              traceM $ "Let block ended due to other token: " ++ show tok
              return []

parseSingleTypeDecl :: Parser TopLevel
parseSingleTypeDecl = do
  st <- get
  traceM $ "\n=== parseSingleTypeDecl ==="
  traceM $ "Current indent: " ++ show (stateIndent st)
  traceM $ "Current tokens: " ++ show (take 3 $ stateTokens st)

  typeNameTok <- matchToken isValidName "type name"
  traceM $ "Found type name: " ++ show typeNameTok

  -- Parse type parameters
  typeParams <- do
    st' <- get
    traceM $ "Checking for type parameters, next tokens: " ++ show (take 3 $ stateTokens st')
    case stateTokens st' of
      (tok : _) | locToken tok == TLeftBracket -> do
        traceM "Found type parameter list"
        _ <- matchToken (== TLeftBracket) "["
        params <- parseTypeParams
        traceM $ "Parsed type parameters: " ++ show params
        _ <- matchToken (== TRightBracket) "]"
        return params
      _ -> do
        traceM "No type parameters found"
        return []

  _ <- matchToken (== TOperator "=") "equals sign"
  case locToken typeNameTok of
    TWord name -> do
      traceM $ "Continuing with type definition for: " ++ name
      typeDefinition <- parseTypeDefinition (T.pack name) typeParams
      return $ TLType name typeDefinition
    _ -> throwError $ UnexpectedToken typeNameTok "type name"

parseTypeDefinition :: T.Text -> [Type] -> Parser Type
parseTypeDefinition givenName params = do
  traceM $ "\n=== parseTypeDefinition ==="
  traceM $ "Parsing type: " ++ T.unpack givenName

  -- Match struct keyword directly - equals was already consumed
  _ <- matchToken (== TStruct) "struct"

  fields <- parseStructFields
  traceM $ "Parsed fields: " ++ show fields

  st <- gets stateSymTable
  traceM $ "Initial symbol table: " ++ show st

  let name = T.unpack givenName
  let (sid, newSt) =
        case params of
          [] -> registerStruct name fields st
          _ -> registerParamStruct name params fields st

  modify $ \s -> s {stateSymTable = newSt}
  return $ TypeStruct sid name

parseStructFields :: Parser [(String, Type)]
parseStructFields = do
  traceM $ "\n=== Parsing struct fields ==="
  -- Get current state for indentation check
  st <- get
  traceM $ "Current state: " ++ show st

  let fieldBaseIndent = case stateTokens st of
        (tok : _) -> locCol tok - 2 -- Base indent is 2 less than field indent
        _ -> 0
  traceM $ "Field base indentation level: " ++ show fieldBaseIndent

  let loop acc = do
        st' <- get
        traceM $ "\n=== Struct fields loop ==="
        traceM $ "Field base indent: " ++ show fieldBaseIndent
        traceM $ "Current tokens: " ++ show (take 3 $ stateTokens st')

        case stateTokens st' of
          (tok : _)
            | isValidName (locToken tok) && locCol tok > fieldBaseIndent -> do
                traceM $ "Parsing field at col " ++ show (locCol tok)
                field <- parseStructField
                traceM $ "Parsed struct field: " ++ show field
                loop (field : acc)
            | locCol tok <= fieldBaseIndent -> do
                traceM $
                  "Found token at col "
                    ++ show (locCol tok)
                    ++ " <= base indent "
                    ++ show fieldBaseIndent
                    ++ ", ending struct fields"
                return $ reverse acc
          _ -> do
            traceM "No more tokens"
            return $ reverse acc

  result <- loop []
  traceM $ "All struct fields parsed: " ++ show result
  return result

parseStructField :: Parser (String, Type)
parseStructField = do
  traceM "\n=== parseStructField ==="
  st <- get
  traceM $ "Current tokens: " ++ show (take 3 $ stateTokens st)

  name <- matchToken isValidName "field name"
  traceM $ "Parsed field name token: " ++ show name

  _ <- matchToken (== TColon) "colon"

  fieldType <- do
    -- Handle field type reference with type param list
    tok <-
      matchToken
        ( \case
            TWord _ -> True
            TTypeParam _ -> True
            _ -> False
        )
        "field type"
    case locToken tok of
      TWord typeName -> do
        nextToks <- gets stateTokens
        case nextToks of
          (tok' : _) | locToken tok' == TLeftBracket -> do
            -- Field type has type params, e.g. Box[T]
            _ <- matchToken (== TLeftBracket) "["
            params <- parseTypeParams
            _ <- matchToken (== TRightBracket) "]"
            case M.lookup typeName (structNames $ stateSymTable st) of
              Just sid -> do
                -- Look up base struct
                case lookupStruct sid (stateSymTable st) of
                  Just def -> do
                    -- Use all type params
                    let typeArgs = params
                    return $ TypeStruct sid typeName
                  Nothing -> throwError $ UnexpectedToken tok "struct type not found"
              Nothing -> return $ TypeUnresolved typeName
          _ -> parseTypeToken tok
      TTypeParam param -> return $ TypeParam param
      _ -> throwError $ UnexpectedToken tok "field type"

  let fieldName = case locToken name of
        TWord fn -> fn
        _ -> error "Invalid field name token"

  traceM $ "Field parsed: " ++ fieldName ++ ": " ++ show fieldType
  traceM $ "Current symbol table state: " ++ show (stateSymTable st)

  return (fieldName, fieldType)

--------------------------------------------------------------------------------

-- | After we've parsed the whole file and pre-registered all struct names,
--   walk the AST and convert any TypeUnresolved to TypeStruct if we can now
--   find it in the symbol table.

--------------------------------------------------------------------------------

resolveUnresolvedTypes :: Program -> SymbolTable -> Program
resolveUnresolvedTypes prg@(Program tops) symtab =
  trace debugMsg $
    Program (map (fixTopLevel symtab) tops)
  where
    debugMsg =
      "\n=== resolveUnresolvedTypes ===\n"
        ++ "Input Program: "
        ++ show prg
        ++ "\n"
        ++ "SymbolTable:\n"
        ++ show symtab
        ++ "\n"
        ++ "structNames keys: "
        ++ show (M.keys (structNames symtab))
        ++ "\n"

--------------------------------------------------------------------------------
fixTopLevel :: SymbolTable -> TopLevel -> TopLevel
fixTopLevel sym tl =
  trace ("\n=== fixTopLevel ===\nTopLevel before fix: " ++ show tl) $
    case tl of
      TLType nm ty ->
        let newTy = fixType ty sym
         in trace
              ( "  fixTopLevel => TLType "
                  ++ nm
                  ++ "\n    oldTy: "
                  ++ show ty
                  ++ "\n    newTy: "
                  ++ show newTy
              )
              $ TLType nm newTy
      TLDecl decl ->
        let newDecl = fixDecl decl sym
         in trace ("  fixTopLevel => TLDecl => " ++ show newDecl) $
              TLDecl newDecl
      TLExpr expr ->
        let newExpr = fixExpr expr sym
         in trace
              ( "  fixTopLevel => TLExpr\n"
                  ++ "    old expr: "
                  ++ show expr
                  ++ "\n    new expr: "
                  ++ show newExpr
              )
              $ TLExpr newExpr

--------------------------------------------------------------------------------
fixDecl :: Decl -> SymbolTable -> Decl
fixDecl oldDecl@(DFunc fname typeParams params retType body) sym =
  trace ("\n=== fixDecl (DFunc) ===\noldDecl: " ++ show oldDecl) $
    let newParams = map (fixParam sym) params
        newRetType = fixType retType sym
        newBody = fixExpr body sym
        newDecl = DFunc fname typeParams newParams newRetType newBody
     in trace ("  fixDecl => newDecl: " ++ show newDecl) newDecl
fixDecl other _ =
  trace ("fixDecl => passing through non-DFunc: " ++ show other) other

--------------------------------------------------------------------------------
fixParam :: SymbolTable -> Param -> Param
fixParam sym p@(Param nm ty) =
  let newTy = fixType ty sym
   in trace
        ( "\n=== fixParam ===\nParam: "
            ++ show p
            ++ "\n => newTy: "
            ++ show newTy
        )
        $ Param nm newTy

--------------------------------------------------------------------------------
fixType :: Type -> SymbolTable -> Type
fixType t@(TypeUnresolved name) sym =
  let debugPrefix = "\n=== fixType (TypeUnresolved) ===\n"
      debugMsg =
        debugPrefix
          ++ "Unresolved type name: "
          ++ name
          ++ "\nSymbolTable structNames keys: "
          ++ show (M.keys (structNames sym))
   in trace debugMsg $
        case M.lookup name (structNames sym) of
          Just sid ->
            trace
              ( "   -> Found sid = "
                  ++ show sid
                  ++ ", returning TypeStruct "
                  ++ name
              )
              $ TypeStruct sid name
          Nothing ->
            trace
              ( "   -> STILL UNRESOLVED => "
                  ++ show name
                  ++ " not found in structNames!"
              )
              $ t -- keep it as TypeUnresolved name
fixType (TypeArray inner) sym =
  trace ("\n=== fixType (TypeArray) ===\n  inner: " ++ show inner) $
    TypeArray (fixType inner sym)
fixType t@(TypeStruct sid n) _ =
  trace
    ( "\n=== fixType => TypeStruct => sid:"
        ++ show sid
        ++ ", name:"
        ++ n
    )
    t
fixType (TypeNum nt) _ =
  trace ("\n=== fixType => TypeNum => " ++ show nt) (TypeNum nt)
fixType (TypeVec vt) _ =
  trace ("\n=== fixType => TypeVec => " ++ show vt) (TypeVec vt)
fixType TypeString _ =
  trace "\n=== fixType => TypeString" TypeString
fixType TypeBool _ =
  trace "\n=== fixType => TypeBool" TypeBool
fixType TypeVoid _ =
  trace "\n=== fixType => TypeVoid" TypeVoid
fixType TypeAny _ =
  trace "\n=== fixType => TypeAny" TypeAny
fixType (TypeParam p) _ =
  trace ("\n=== fixType => TypeParam => " ++ p) (TypeParam p)
fixType (TypeOption inner) sym =
  trace ("\n=== fixType => TypeOption => " ++ show inner) TypeOption (fixType inner sym) -- Recursively fix inner type
  --------------------------------------------------------------------------------

fixExpr :: Expr -> SymbolTable -> Expr
fixExpr (ArrayLit ty es) sym =
  let newTy = fixType ty sym
      newEs = map (`fixExpr` sym) es
   in trace
        ( "\n=== fixExpr (ArrayLit) ===\n"
            ++ " old array type: "
            ++ show ty
            ++ "\n new array type: "
            ++ show newTy
            ++ "\n old #elems: "
            ++ show (length es)
        )
        $ ArrayLit newTy newEs
fixExpr (StructLit nm fields) sym =
  let newFields = [(fname, fixExpr fval sym) | (fname, fval) <- fields]
   in trace
        ( "\n=== fixExpr (StructLit) => name: "
            ++ nm
            ++ "\n old fields: "
            ++ show fields
            ++ "\n new fields: "
            ++ show newFields
        )
        $ StructLit nm newFields
fixExpr (Block lbl exprs mres) sym =
  let newExprs = map (`fixExpr` sym) exprs
      newRes = fmap (`fixExpr` sym) mres
   in trace
        ( "\n=== fixExpr (Block) => label: "
            ++ lbl
            ++ "\n #exprs => "
            ++ show (length exprs)
        )
        $ Block lbl newExprs newRes
fixExpr other _ =
  trace ("\n=== fixExpr => passing through: " ++ show other) other

-- After you fix the AST, fix all structs in symbolDefs
fixSymbolTableTypes :: SymbolTable -> SymbolTable
fixSymbolTableTypes sym =
  let oldStructDefs = structDefs sym
      newStructDefs = M.map (fixStructDef sym) oldStructDefs
      oldFuncDefs = funcDefs sym
      newFuncDefs = M.map (fixFuncDef sym) oldFuncDefs
   in trace
        ( "\n=== fixSymbolTableTypes === "
            ++ "\n oldStructDefs => "
            ++ show oldStructDefs
            ++ "\n newStructDefs => "
            ++ show newStructDefs
            ++ "\n oldFuncDefs => "
            ++ show oldFuncDefs
            ++ "\n newFuncDefs => "
            ++ show newFuncDefs
        )
        $ sym {structDefs = newStructDefs, funcDefs = newFuncDefs}

fixStructDef :: SymbolTable -> StructDef -> StructDef
fixStructDef sym oldDef =
  let oldFields = structFields oldDef
      newFields = [(fName, fixType fType sym) | (fName, fType) <- oldFields]
   in trace
        ( "\n=== fixStructDef === "
            ++ "\n oldFields => "
            ++ show oldFields
            ++ "\n newFields => "
            ++ show newFields
        )
        $ oldDef {structFields = newFields}

fixFuncDef :: SymbolTable -> FunctionDef -> FunctionDef
fixFuncDef sym oldDef =
  let oldParams = funcParams oldDef
      newParams = map (fixParam sym) oldParams
      newRetType = fixType (funcRetType oldDef) sym
   in trace
        ( "\n=== fixFuncDef === "
            ++ "\n oldParams => "
            ++ show oldParams
            ++ "\n newParams => "
            ++ show newParams
        )
        $ oldDef {funcParams = newParams, funcRetType = newRetType}
