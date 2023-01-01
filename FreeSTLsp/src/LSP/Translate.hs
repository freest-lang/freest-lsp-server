{-# LANGUAGE OverloadedStrings #-}

module LSP.Translate
    ( errorTypeToDiagnostic
    , spanToRange
    , dataToSession
    , dataToPrettySession
    )
    where

-- LSP
import           Language.LSP.Types 
-- FreeST LSP
import           LSP.Util ( buildDiagnostic )
-- FreeST
import           Syntax.Base
import           Syntax.Type
import           Util.Error       (ErrorType, showErrors)
-- import           Util.Warning     (WarningType, showWarnings)
import           Util.FreestState (FreestS (runOpts, typenames), RunOpts (runFilePath))
-- Others
import           Data.Text (pack)
import           Data.List (intercalate)
import           Data.Map.Strict as Map
import           Debug.Trace (trace)

-- | FreeST -> LSP

errorTypeToDiagnostic :: FreestS -> ErrorType -> Diagnostic
errorTypeToDiagnostic s err = 
    buildDiagnostic 
        -- False -> isStylable, for no colors (prevents weird codes) 
        (showErrors False (runFilePath $ runOpts s) (typenames s) err) 
        (spanToRange $ getSpan err)

-- warningTypeToDiagnostic :: FreestS -> WarningType -> Diagnostic
-- warningTypeToDiagnostic s wrn =
--     buildDiagnostic 
--         (showWarnings (runFilePath $ runOpts s) (typenames s) wrn)
--         (spanToRange $ getSpan wrn)

posToPosition :: Pos -> Position
posToPosition (line, column) = Position (line-1) (column-1)

spanToRange :: Span -> Range
spanToRange (Span startPos endPos _) = 
    Range (posToPosition startPos) (posToPosition endPos)



-- | FreeST Datatype -> FreeST Session Type

dataToSession :: TypeMap -> Type -> (TypeMap, Type, [(String, Type)])
-- | Basic types
dataToSession ctx (Int s)    = (ctx, sendT (Int s)   , [])
dataToSession ctx (Char s)   = (ctx, sendT (Char s)  , [])
dataToSession ctx (Bool s)   = (ctx, sendT (Bool s)  , [])
dataToSession ctx (String s) = (ctx, sendT (String s), [])
dataToSession ctx (Unit s)   = (ctx, sendT (Unit s)  , [])
dataToSession ctx (Pair s t1 t2) = 
    let (ctx1, t1', ts1) = dataToSession ctx t1 in
    let (ctx2, t2', ts2) = dataToSession ctx1 t2 in
    (ctx2, Semi s t1' t2', ts1 ++ ts2)
-- | Datatype constructors are arrow functions
dataToSession ctx (Arrow s _ t1 t2) = 
    let (ctx1, t1', ts1) = dataToSession ctx t1 in
    let (ctx2, t2', ts2) = dataToSession ctx1 t2 in
    (ctx2, Semi s t1' t2', ts1 ++ ts2)
-- | Mantain skips inserted by norm function
dataToSession ctx (Skip s) = (ctx, Skip s, [])
-- | Forall might come in future poly datatypes
-- dataToSession _ (Forall _ _) = undefined
-- | Rec types are datatypes
dataToSession ctx (Rec s (Bind sBind (Variable vSpan name) binder (Almanac sA sort tMap))) = 
  let stName = name ++ "C" in
  let stVar = mkVar vSpan stName in
  if stVar `Map.member` ctx
  then
    -- Session type already exists, no need to create
    (ctx, (Var vSpan stVar), [])
  else
    -- Need to create a new session type
    let normedTMap = Map.map (norm (Variable s name)) tMap in
    let (ctx', tMap', newTypes) = Map.foldrWithKey f (Map.insert stVar (Int defaultSpan) ctx, Map.empty, []) normedTMap in

    (Map.insert stVar (Almanac s (Choice Internal) tMap') ctx', (Var vSpan stVar), (stName, Almanac s (Choice Internal) tMap') : newTypes)
    where
        f var t (ctx, tMap, newTypes) =
            let (ctx', t', newTypes') = dataToSession ctx t in
            (ctx', Map.insert var t' tMap, newTypes' ++ newTypes)
dataToSession ctx (Var s var) = (ctx, (Var s (Variable s $ intern var ++ "C")), [])
-- | Trace any uncaught case 
-- dataToSession _ t = trace ("Error: " ++ show t) t

norm :: Variable -> Type -> Type
norm v (Arrow s1 m1 t1 (Arrow s2 m2 t2 t3)) = (Arrow s1 m1 t1 (norm v (Arrow s2 m2 t2 t3)))
norm _ (Arrow s _ t1 t2) = t1
norm v (Var s var) = if v == var then (Skip defaultSpan) else (Var s var)
norm _ t = t

sendT :: Type -> Type
sendT t = Message (getSpan t) Out t

dataToPrettySession :: TypeMap -> Type -> String
dataToPrettySession ctx t = 
    let (_, _, newTypes) = dataToSession ctx t in
    trace (show newTypes) $ (++ "\n") $ intercalate "\n" $ Prelude.map f newTypes
    where
        f (s, t) = "type " ++ s ++ " : 1S = " ++ show t