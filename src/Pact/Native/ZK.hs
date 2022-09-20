{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      :  Pact.Native.ZK
-- Copyright   :  (c) 2022 Stuart Popejoy, Emily Pillmore
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Emily Pillmore <emily@kadena.io>
--                Jose Cardona <jose@kadena.io>,
--                John Wiegley <john@kadena.io>
--
-- Builtins for working with ZK proofs.
--
module Pact.Native.ZK
  ( zkDefs
  ) where

import Control.Lens (view)

import Data.Default

import Pact.Native.Internal
import Pact.Types.ZK
import Pact.Types.Pretty
import Pact.Types.Runtime


zkDefs :: NativeModule
zkDefs =
  ("ZK",
   [ verifyZK
   ])

verifyZK :: NativeDef
verifyZK =
  defRNative "verify-zk" verifyZK'
  (funType tTyBool
   [("type", tTyString),
    ("payload", tTyString)])
  [LitExample "(verify-zk \"ELECTRON\" (read-msg \"proof\"))"]
  "Performs a platform-specific zk proof of type TYPE on PAYLOAD. \
  \The format of the PAYLOAD object is TODO. Platforms such as Chainweb will \
  \document the specific payload types."
  where
    verifyZK' i [TLitString proofType, TLitString proof] = do
      view (eeZKSupport . zkSupport) >>= \f -> liftIO (f proofType proof) >>= \case
        Left err -> evalError' i $ "ZK verify failed: " <> pretty err
        Right b -> pure $ TLiteral (LBool b) def
    verifyZK' i as = argsError i as
