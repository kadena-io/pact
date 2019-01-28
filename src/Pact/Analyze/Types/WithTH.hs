{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE Rank2Types      #-}
{-# LANGUAGE TemplateHaskell #-}
module Pact.Analyze.Types.WithTH where

import Language.Haskell.TH

import           Data.Constraint              (Dict (Dict), withDict)
import Pact.Analyze.Types.Types

declareWith :: String -> Q [Dec]
declareWith classStrName = do
  let -- className = mkName classStrName
      funName = mkName $ "with" ++ classStrName
      helperName = mkName $ "singMk" ++ classStrName

  -- t <- sigD funName [t| forall a b. SingTy a -> ($className (Concrete a) => b) -> b |]

  d <- funD funName
    [ clause
      []
      (normalB [| withDict . $(varE helperName) |])
      [ funD helperName
        [ clause
          []
          (normalB [|
            \case
              SInteger   -> Dict
              SBool      -> Dict
              SStr       -> Dict
              STime      -> Dict
              SDecimal   -> Dict
              SGuard     -> Dict
              SAny       -> Dict
              SList ty'  -> $(varE funName) ty' $ withTypeable ty' Dict
              SObjectUnsafe SNil' -> Dict
              SObjectUnsafe (SCons' _ ty' tys)
                -> $(varE funName) ty' $
                   withDict ($(varE helperName) (SObjectUnsafe tys)) Dict
          |])
          []
        ]
      ]
    ]
  pure [d]
