{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}


-- |
-- Module      :  Pact.Native.SPV
-- Copyright   :  (C) 2019 Stuart Popejoy
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Stuart Popejoy <stuart@kadena.io>
--
-- Builtins for working with SPV proofs.
--

module Pact.Native.SPV
  ( spvDefs
  ) where

import Control.Monad.IO.Class (liftIO)
import Control.Lens (view)
import Pact.Native.Internal
import Pact.Types.Runtime
import Pact.Types.Pretty
import Data.Default


spvDefs :: NativeModule
spvDefs =
  ("SPV",
   [ verifySPV
   ])

verifySPV :: NativeDef
verifySPV =
  defRNative "verify-spv" verifySPV'
  (funType (tTyObject (mkTyVar' "out"))
   [("type", tTyString),
    ("payload", tTyObject (mkTyVar' "in"))])
  [LitExample "(verify-spv \"TXOUT\" (read-msg \"proof\"))"]
  "Performs a platform-specific spv proof of type TYPE on PAYLOAD. \
  \The format of the PAYLOAD object depends on TYPE, as does the \
  \format of the return object. Platforms such as Chainweb will \
  \document the specific payload types and return values."
  where
    verifySPV' i [TLitString proofType, TObject{..}] = do
      view eeSPVSupport >>= \(SPVSupport f) -> liftIO (f proofType _tObject) >>= \r -> case r of
        Left err -> evalError' i $ "SPV verify failed: " <> pretty err
        Right o -> return $ TObject o def
    verifySPV' i as = argsError i as
