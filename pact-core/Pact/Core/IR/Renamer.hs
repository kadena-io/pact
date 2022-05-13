{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Pact.Core.IR.Renamer where

-- import Control.Monad.Reader
-- import Data.Map(Map)
-- import qualified Data.Map.Strict as Map

-- import Pact.Core.Names
-- import Pact.Core.IR.Term
-- import Pact.Core.Type

type RenamerT = ReaderT Supply IO


-- renameTerm :: Term ParsedName Text RawBuiltin i -> RenamerT (Term IRName TypeVar i)
-- renameTerm = \case
--   Var (BN (BareName n)) -> do
--     env <- view rnBinds
--     case Map.lookup n env of
--       Just (u, nk) ->
--         pure (Var (IRName n nk u) i)
--       Nothing -> error "unbound free variable"
--   Lam (BN (BareName lamName))
