{-# LANGUAGE LambdaCase #-}

module Pact.Core.Typed.Term where

import Control.Lens
import Data.Map(Map)
import qualified Data.Map.Strict as M
import Data.Text(Text)
import Data.Vector (Vector)
import qualified Data.Vector as V


import Pact.Core.Names
import Pact.Core.Type
import Pact.Types.Exp (Literal)

-- | Typed pact core terms
data Term name tyname builtin info
  = Var name info
  | Lam name (Type tyname) (Term name tyname builtin info) info
  | App (Term name tyname builtin info) (Term name tyname builtin info) info
  | TyApp (Term name tyname builtin info) (Type tyname) info
  | TyAbs tyname (Term name tyname builtin info) info
  | ObjectLit (Row tyname) (Map Field (Term name tyname builtin info)) info
  | ListLit (Type tyname) (Vector (Term name tyname builtin info)) info
  | Error String (Type tyname) info
  | Builtin builtin info
  | Constant Literal info
  deriving (Show)

termInfo :: Lens' (Term name tyname builtin info) info
termInfo f = \case
  Var n i -> Var n <$> f i
  Lam n ty term i -> Lam n ty term <$> f i
  App t1 t2 i -> App t1 t2 <$> f i
  TyApp term ty i -> TyApp term ty <$> f i
  TyAbs ty term i -> TyAbs ty term <$> f i
  ObjectLit row obj info -> ObjectLit row obj <$> f i
  ListLit ty v i -> ListLit ty v <$> i
  Error s ty i -> Error s ty <$> f i
  Builtin b i -> Builtin b <$> f i
  Constant l i -> Constant l <$> f i

instance Plated (Term name tyname builtin info) where
  plate f = \case
    Var n i -> pure (Var n i)
    Lam n ty term i -> Lam n ty <$> f term <*> pure i
    App t1 t2 i -> App <$> f t1 <*> f t2 <*> pure i
    TyApp term ty i -> TyApp <$> f term <*> pure ty <*> pure i
    TyAbs ty term i -> TyAbs ty <$> f term <*> pure i
    Error s ty i -> pure (Error s ty i)
    Builtin b i -> pure (Builtin b i)
    Constant l i -> pure (Constant l i)
