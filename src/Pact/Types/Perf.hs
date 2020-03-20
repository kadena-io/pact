{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      :  Pact.Types.Perf
-- Copyright   :  (C) 2020 Stuart Popejoy
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Stuart Popejoy <stuart@kadena.io>
--
-- Perf measurement stuff.
--

module Pact.Types.Perf
  ( PerfTimer(..)
  , perf
  ) where

import Control.Monad.IO.Class
import Data.Default
import Data.Text (Text)

data PerfTimer = PerfTimer { _perfTimer :: forall m a . MonadIO m => Text -> m a -> m a }

instance Default PerfTimer where def = PerfTimer (const id)
instance Show PerfTimer where show _ = "PerfTimer"

perf :: MonadIO m => PerfTimer -> Text -> m a -> m a
perf (PerfTimer t) msg act = t msg act
