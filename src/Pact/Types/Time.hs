{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module: Pact.Types.Time
-- Copyright: Copyright © 2018 Kadena LLC
-- License:  BSD-style (see the file LICENSE)
-- Maintainer:  Stuart Popejoy <stuart@kadena.io>
--
-- Time functions for use with pact
--
-- This module is a compatibility layer between the time
-- functions used in the pact code base and supported
-- third-party time libraries.
--
-- The main reason for the existence of this module is
-- the lack of maintenance for the thyme library, the
-- uncertainty of its future, and the lack of good
-- alternatives. When this situation is resolved this
-- module and the associated modules under
-- @Pact.Types.TimeCompat@ should be removed.
--
-- For the time being the module shields the Pact code base
-- from the ongoing (unversioned) churn of the different
-- versions of thyme and guarantees stable behavior.
--
-- Currently the following libraries are supported:
--
-- [thyme ≥ 0.3.6.0]
--     A fork from the thyme master branch that is available
--     at https://github.com/kadena-io/thyme.
--
--     Directoy supports all pact time functionality with an
--     efficient implementation.
--
--     No third party or community support.
--
-- [thyme master]
--    The master branch of the thyme library as of 2018-04-19.
--
--     Directoy supports all pact time functionality with an
--     efficient implementation.
--
--     Doesn't support compilation with GHCJS on MacOSX.
--
--     There isn't an official package on Hackage for this version
--     of the library. Therefor no stability is guaranteed.
--
--     It appears that the thyme library isn't actively supported
--     or developed any more. So there may never be a new
--     official version that would include the new features of this
--     branch.
--
-- [thyme ≥ 0.3.5]
--    The most recent released version of the thyme package on
--    hackage as of 2018-04-19.
--
--    Requires some lifting to achieve compatibility.
--
--    More efficient implementation than the time library.
--
--     It appears that the thyme library isn't actively supported
--     or developed any more. So this version may not receive
--     any updates or bug fixes.
--
-- [time ≥ 1.5]
--    The de facto standard time library for haskell.
--
--    Requires more heavy lifting to achieve compatibility than
--    more recent versions (≥ 1.9) of the time library.
--
-- [time ≥ 1.9]
--    The most recent version (as of 2018-04-19) of the defacto
--    standard time librar for haskell.
--
--    Requires some straight forward lifting to achieve
--    compatibility.
--
--    Less efficient implementation than the thyme library.
--
--    Most stable and best supported and maintained time library.
--
module Pact.Types.Time
( module Implementation
) where

#ifdef USE_THYME
import Pact.Types.TimeCompat.Thyme as Implementation
#else
import Pact.Types.TimeCompat.Time as Implementation
#endif

