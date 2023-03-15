{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}
{-# OPTIONS_GHC -Wno-missing-fields #-}

{-# LANGUAGE BlockArguments #-}

module FreeSTLspServer where

-- LSP
import Language.LSP.Server

-- FreeST LSP
import LSP.Handler
    ( handleInitialized
    , handleWorkspaceDidChangeWatchedFiles
    , handleTextDocumentHover
    --, handleTextDocumentCodeAction
    )

import LSP.FreestLspM

-- FreeST
import Util.FreestState

-- Haskell
import Control.Monad.IO.Class ( MonadIO(liftIO) )
import Control.Monad.State
import Control.Concurrent.MVar
import System.IO ( hPutStrLn, stderr )

handlers :: Handlers (FreestLspM ())
handlers = mconcat
  [ handleInitialized
  , handleWorkspaceDidChangeWatchedFiles
  --, handleTextDocumentHover
  --, handleTextDocumentCodeAction
  ]


main :: IO Int
main =
  hPutStrLn stderr "FreeST LSP server connected." >>
  newMVar Nothing >>= \state ->
  runServer $ ServerDefinition
  { onConfigurationChange = const $ pure $ Right ()
  , doInitialize = \env _req -> pure $ Right env
  , staticHandlers = handlers
  , interpretHandler = \env -> Iso (forward env state) liftIO
  , options = defaultOptions
  }
  where 
    forward :: LanguageContextEnv config -> MVar (Maybe FreestS) -> FreestLspM config a -> IO a
    forward env state m =
      modifyMVar state \oldState ->
        runLspT env $ runStateT m oldState >>= \(e, newState) -> return (newState, e)