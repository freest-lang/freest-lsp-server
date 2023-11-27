{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module LSP.FreestLspM where


-- LSP
import           Language.LSP.Server
-- FreeST
import           Util.State ( FreestS )
import           Validation.Phase ( Typing )
-- Haskell
import           Control.Monad.State

type FreestLspM config = StateT (Maybe (FreestS Typing)) (LspT config IO)