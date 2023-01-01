{-# LANGUAGE NamedFieldPuns #-}

module LSP.Check
    ( checkForErrors )
    where

-- LSP
import qualified Language.LSP.Types as LSP

-- FreeST
import           Elaboration.Elaboration ( elaboration )
import           Interpreter.Builtin ( initialCtx )
import           Interpreter.Eval ( evalAndPrint )
import           Parse.Parser ( parseProgram, parseAndImport )
import           Util.CmdLine
import           Util.FreestState
import           Syntax.Base
import           Syntax.Program (noConstructors, VarEnv)
import           Util.Error
-- import           Util.Warning
import           Validation.Rename ( renameState )
import           Validation.TypeChecking ( typeCheck )

import           Control.Monad.State ( when, unless, execState )
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import           Paths_FreeST ( getDataFileName )
import           System.Exit ( die )

-- FreeST LSP
import           LSP.Translate (errorTypeToDiagnostic)

-- Others
import           Control.Monad.State (when, execState)
import qualified Data.Map as Map
import           GHC.IO              (unsafePerformIO)

import Debug.Trace (traceM, trace)


checkForErrors :: FilePath -> Either [LSP.Diagnostic] FreestS
checkForErrors = unsafePerformIO . checkForParseErrors


checkForParseErrors :: FilePath -> IO (Either [LSP.Diagnostic] FreestS)
checkForParseErrors filePath = do
    let runOpts = defaultOpts
    -- | Prelude
    preludeFp <- getDataFileName "Prelude.fst"
    let s0 = initialState {runOpts=runOpts{runFilePath=preludeFp}}
    s1 <- preludeHasErrors (runFilePath runOpts) s0 <$> parseProgram s0

    -- | Prelude entries without body are builtins  
    let venv = Map.keysSet (noConstructors (typeEnv s1) (varEnv s1))
    let penv = Map.keysSet (parseEnv s1)
    let bs = Set.difference venv penv

    -- Parse
    s2 <- parseAndImport s1{builtins=bs, runOpts=runOpts{runFilePath=filePath}}
    if hasErrors s2 
    then return $ Left $ map (errorTypeToDiagnostic s2) (errors s2)
    else checkTypeDecl s2 runOpts filePath
    
    where
        preludeHasErrors :: FilePath -> FreestS -> FreestS -> FreestS
        preludeHasErrors f s0 s1
            | hasErrors s1 = s0 { warnings = {-NoPrelude f :-} warnings s0 }
            | otherwise    = s1

    

checkTypeDecl :: FreestS -> RunOpts -> FilePath -> IO (Either [LSP.Diagnostic] FreestS)
checkTypeDecl s2 runOpts filePath = do
    -- | Solve type declarations and dualof operators
    let s3 = emptyPEnv $ execState elaboration s2
    if hasErrors s2 
    then return $ Left $ map (errorTypeToDiagnostic s3) (errors s3)
    else checkForTypeErrors s3

checkForTypeErrors :: FreestS -> IO (Either [LSP.Diagnostic] FreestS)
checkForTypeErrors s3 = do
    -- | Rename
    let s4 = execState renameState s3

    -- | Type check
    let s5 = execState typeCheck s4
    -- TODO: Add warnings
    -- when (not (quietmode runOpts) && hasWarnings s5) (putStrLn $ getWarnings s5)
    if hasErrors s5 
    then return $ Left $ map (errorTypeToDiagnostic s5) (errors s5)
    else checkFunctionBindings s5

checkFunctionBindings :: FreestS -> IO (Either [LSP.Diagnostic] FreestS)
checkFunctionBindings s5 = do
    -- | Check whether a given function signature has a corresponding
    --   binding
    let venv = Map.keysSet (noConstructors (typeEnv s5) (varEnv s5))
    let p = Map.keysSet (prog s5)
    let bs = Set.difference (Set.difference venv p) (builtins s5)
    
    if Set.null bs
    then return $ Right s5
    else return $ Left $ map (errorTypeToDiagnostic s5) $ errors $ Set.foldr (noSig (varEnv s5)) initialState bs
    where
        noSig :: VarEnv -> Variable -> FreestS -> FreestS
        noSig venv f acc = acc { errors = SignatureLacksBinding (getSpan f) f (venv Map.! f) : errors acc }
