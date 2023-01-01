{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module LSP.FreestLspM where


-- LSP
import           Language.LSP.Server
-- FreeST
import           Util.FreestState ( FreestS )
-- Haskell
import           Control.Monad.State

type FreestLspM config = StateT (Maybe FreestS) (LspT config IO)