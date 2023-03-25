{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}

module LSP.Handler
    ( handleInitialized
    , handleWorkspaceDidChangeWatchedFiles
    , handleTextDocumentHover
    , -- handleTextDocumentCodeAction
    ) where

-- LSP
import           Language.LSP.Server
import           Language.LSP.Types
import           Language.LSP.VFS
import qualified Language.LSP.Types.Lens as Lens
import           Language.LSP.Diagnostics ( partitionBySource )

-- FreeST LSP
import           LSP.Check (checkForErrors)
import           LSP.Translate (spanToRange)
import           LSP.Util
import           LSP.FreestLspM
import           Util.FileUtil

-- FreeST
import           Syntax.Base
import           Syntax.Type as T
import           Syntax.Program

-- Others
import           System.IO.Unsafe (unsafePerformIO)
import           Data.Map.Strict as Map
import           Data.HashMap.Strict as HashMap
import           Data.Maybe
import           Data.Text as Text
import           Data.List ( find )
import           Util.FreestState (getErrors, FreestS(typeEnv, typenames))
import           Debug.Trace      (traceM, trace)
import           Control.Monad.Trans (lift)
import           Control.Monad.Trans.State


liftLSP :: LspM config a -> FreestLspM config a
liftLSP m = lift m

--------------------------------------------------------------------------
------------------------------ NOTIFICATION ------------------------------
--------------------------------------------------------------------------

handleInitialized :: Handlers (FreestLspM ())
handleInitialized =
    notificationHandler SInitialized $ \_not -> do
      -- DEBUG ONLY
      -- traceM "FreeST LSP server initialized"
      pure ()

handleWorkspaceDidChangeWatchedFiles :: Handlers (FreestLspM ())
handleWorkspaceDidChangeWatchedFiles =
   notificationHandler SWorkspaceDidChangeWatchedFiles $ \_not -> do
    let (NotificationMessage _ j params) = _not
    let (DidChangeWatchedFilesParams (List ((FileEvent uri _) : changes))) = params
    let version = Nothing
    
    -- DEBUG ONLY
    -- traceM (show diagnostics)
    -- traceM (show j)

    case checkForErrors $ fromJust $ uriToFilePath uri of
      Left diagnostics -> do
        -- Flush old diagnostics
        _ <- liftLSP $ flushDiagnosticsBySource 100 (Just "freest-lsp")
        _ <- put Nothing
        -- Errors, send them
        _ <- liftLSP $ publishDiagnostics 
          100 
          (toNormalizedUri uri) 
          version 
          (partitionBySource diagnostics)
        
        pure ()
      Right state -> do
        _ <- put (Just state)
        -- No errors, clear any in the client
        _ <- liftLSP $ flushDiagnosticsBySource 100 (Just "freest-lsp")
        pure ()

--------------------------------------------------------------------------
-------------------------------- REQUEST ---------------------------------
--------------------------------------------------------------------------

handleTextDocumentHover :: Handlers (FreestLspM ())
handleTextDocumentHover =
  requestHandler STextDocumentHover $ \req responder -> do
    let RequestMessage _ _ _ (HoverParams (TextDocumentIdentifier uri) (Position l c) _workDone) = req
    maybeState <- get
    word <- liftLSP (lift (getWordFromFile (l, c) (fromJust $ uriToFilePath uri)))

    case maybeState of
      Nothing -> emptyResponse responder
      Just state -> do
        case (typeEnv state) Map.!? mkVar defaultSpan word of
          Nothing -> trace ("Hover: No type found for '" ++ word ++ "'") emptyResponse responder
          Just (k, t) -> do
            case (typenames state) Map.!? getSpan t of
              Nothing -> trace ("Hover: No typename found for '" ++ word ++ "'") emptyResponse responder
              Just t -> responder $ Right $ Just $ Hover (HoverContents $ MarkupContent MkPlainText $ Text.pack (show t)) $ Nothing
  where
    emptyResponse responder = responder $ Right $ Nothing


-- handleTextDocumentCodeAction :: Handlers (FreestLspM ())
-- handleTextDocumentCodeAction =
--   requestHandler STextDocumentCodeAction $ \req responder -> do
--     let RequestMessage _ _ _ (CodeActionParams _ _ (TextDocumentIdentifier uri) range _) = req
    
--     maybeState <- get
--     case maybeState of
--       Nothing ->
--         emptyResponse responder
--       Just state -> do
--         let tEnv = typeEnv state
--         case Data.List.find (isRangeInsideRange range . spanToRange . getSpan) $ Map.keys tEnv of
--           Nothing -> 
--             emptyResponse responder
--           Just var ->
--             let t = snd $ tEnv Map.! var in
--             if not $ isDatatype t
--             then emptyResponse responder
--             else do
--               let prettySessions = dataToPrettySession (Map.map snd tEnv) t
--                   textEdit = TextEdit originRange $ Text.pack prettySessions
--                   workspaceEdit = WorkspaceEdit (Just $ HashMap.singleton uri $ List [textEdit]) 
--                                                 Nothing
--                                                 Nothing
--               if prettySessions == "\n"
--               then emptyResponse responder
--               else do
--                 responder $ Right $ List $ 
--                   [InR $ 
--                     CodeAction (Text.pack $ "Generate Session Type: " ++ intern var ++ "C")
--                               (Just CodeActionRefactor)
--                               Nothing 
--                               (Just True) 
--                               Nothing 
--                               (Just workspaceEdit) 
--                               Nothing 
--                               Nothing
--                   ]
--                 -- responder (Left $ ResponseError RequestCancelled "not implemented" Nothing)
--   where
--     emptyResponse responder = responder $ Right $ List []

--     isDatatype :: T.Type -> Bool
--     isDatatype (T.Rec _ (Bind _ _ _ t))  = isDatatype t
--     isDatatype (T.Almanac _ T.Variant m) = True --c `Map.member` m
--     isDatatype _                         = False
