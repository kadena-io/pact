{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
-- |
-- Module      :  Pact.Types.ZK
-- Copyright   :  (c) 2022 Stuart Popejoy, Emily Pillmore
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Emily Pillmore <emily@kadena.io>,
--                Jose Cardona <jose@kadena.io>,
--                John Wiegley <john@kadena.io>
--
-- SPV Support data and types
--
module Pact.Types.ZK
( ZKSupport(..)
, zkSupport
, noZKSupport
)where


import Control.Lens

import Data.Text

-- | Backend for zero-knowledge proof support.
--
-- This is analogous to SPV support as described in 'SPVSupport'. Hooks into the
-- 'verify-zk' native function.
--
newtype ZKSupport = ZKSupport
  { _zkSupport :: Text -> Text -> IO (Either Text Bool) }
makeLenses ''ZKSupport

-- | Explicit non-support for zero-knowledge proofs
--
noZKSupport :: ZKSupport
noZKSupport = ZKSupport $ \_ _ -> pure $ Left "ZK verification not supported"
