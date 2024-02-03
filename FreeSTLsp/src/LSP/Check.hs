{-# LANGUAGE NamedFieldPuns #-}

module LSP.Check
    ( checkForErrors )
    where

-- LSP
import qualified Language.LSP.Types as LSP

-- FreeST
import PatternMatch.PatternMatch

import           Elaboration.Elaboration -- ( elaboration )
import           Elaboration.Phase
import           Interpreter.Eval ( evalAndPrint )
import           Parse.Parser ( parseProgram, parseAndImport )
import           Parse.Phase
import           Syntax.AST
import           Syntax.Base
import qualified Syntax.Expression as E
import           Syntax.MkName
import           Syntax.Program (noConstructors)
import           Util.CmdLine
import           Util.Error
import           Util.State
import           Util.Warning
import           Typing.Phase
import           Typing.Rename ( renameProgram )
import           Typing.Typing ( typeCheck )
import           PatternMatch.Phase ( PatternMatch )

import           Control.Monad.State hiding (void)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import           Paths_FreeST ( getDataFileName )

-- FreeST LSP
import           LSP.Translate (errorTypeToDiagnostic)

-- Others
import           Control.Monad.State (when, execState)
import qualified Data.Map as Map
import           GHC.IO              (unsafePerformIO)

import Debug.Trace (traceM, trace)
import Language.LSP.Types (filePathToUri)


checkForErrors :: FilePath -> Either [LSP.Diagnostic] (FreestS Typing)
checkForErrors = unsafePerformIO . checkForParseErrors


checkForParseErrors :: FilePath -> IO (Either [LSP.Diagnostic] (FreestS Typing))
checkForParseErrors filePath = do
    let runOpts = defaultOpts{runFilePath=filePath}
    -- | Prelude
    s0 <- initialWithFile <$> getDataFileName "Prelude.fst"
    s1 <- preludeHasErrors (runFilePath runOpts) s0 <$> parseProgram s0

    -- | Prelude entries without body are builtins  
    let sigs = Map.keysSet (noConstructors (getTypesS s1) (getSignaturesS s1))
    let penv = Map.keysSet (getDefsS s1)
    let bs = Set.difference sigs penv

    -- Parse
    -- | Parse
    s2 <- parseAndImport s1{extra = (extra s1){runOpts}}

    if hasErrors s2 
    then return $ Left $ map (errorTypeToDiagnostic s2 runOpts) (errors s2)
    else checkPatternMatching s2 runOpts bs
    
    where
        preludeHasErrors :: FilePath -> FreestS Parse -> FreestS Parse -> FreestS Parse
        preludeHasErrors f s0 s1
            | hasErrors s1 = s0 { warnings = {-NoPrelude f :-} warnings s0 }
            | otherwise    = s1


checkPatternMatching :: FreestS Parse -> RunOpts -> Set.Set Variable -> IO (Either [LSP.Diagnostic] (FreestS Typing))
checkPatternMatching s2 runOpts bs = do
    -- | PatternMatch
    let patternS = patternMatch s2

    if hasErrors patternS
    then return $ Left $ map (errorTypeToDiagnostic patternS runOpts) (errors patternS)
    else checkElaboration patternS runOpts bs


checkElaboration :: FreestS PatternMatch -> RunOpts -> Set.Set Variable -> IO (Either [LSP.Diagnostic] (FreestS Typing))
checkElaboration patternS runOpts bs = do
    -- | Elaboration
    let (defs, elabS) = elaboration patternS

    if hasErrors elabS
    then return $ Left $ map (errorTypeToDiagnostic elabS runOpts) (errors elabS)
    else checkForTypeErrors elabS runOpts bs defs


checkForTypeErrors :: FreestS Elab -> RunOpts -> Set.Set Variable -> Definitions Typing -> IO (Either [LSP.Diagnostic] (FreestS Typing))
checkForTypeErrors elabS runOpts bs defs = do
    -- | Rename & TypeCheck
    let s4 = execState (renameProgram >> typeCheck) (elabToTyping runOpts defs elabS)

    -- TODO: warnings
    -- when (not (quietmode runOpts) && hasWarnings s4) (putStrLn $ getWarnings runOpts s4)

    if hasErrors s4
    then return $ Left $ map (errorTypeToDiagnostic s4 runOpts) (errors s4)
    else checkFunctionBindings s4 runOpts bs



checkFunctionBindings :: FreestS Typing -> RunOpts -> Set.Set Variable -> IO (Either [LSP.Diagnostic] (FreestS Typing))
checkFunctionBindings s4 runOpts bs = do
    -- | Check whether a given function signature has a corresponding
    --   binding
    let sigs = Map.keysSet (noConstructors (types $ ast s4) (signatures $ ast s4))
    let p = Map.keysSet (definitions $ ast s4)
    let bs1 = Set.difference (Set.difference sigs p) bs -- (builtins s4)
    
    if Set.null bs
    then return $ Right s4
    else return $ Left $ map (errorTypeToDiagnostic s4 runOpts) $ Set.foldr (noSig (getSignaturesS s4)) [] bs1
    where
        noSig :: Signatures -> Variable -> Errors -> Errors
        noSig sigs f acc = SignatureLacksBinding (getSpan f) f (sigs Map.! f) : acc

elabToTyping :: RunOpts -> Typing.Phase.Defs -> ElabS -> TypingS
elabToTyping runOpts defs s = s {ast=newAst, extra = runOpts}
  where newAst = AST {types=types $ ast s, signatures=signatures $ ast s, definitions = defs}