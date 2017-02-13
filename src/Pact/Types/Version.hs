{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
-- |
-- Module     : Pact.Types.Version
-- Copyright  : (C) 2017 Stuart Popejoy
-- License    : BSD-style (see the file LICENSE)
-- Maintainer : Stuart Popejoy <stuart@kadena.io>
--
-- Uses CPP to access PACT_VERSION from cabal.
--
module Pact.Types.Version (pactVersion) where

import Data.Text (Text)

pactVersion :: Text
pactVersion = PACT_VERSION
