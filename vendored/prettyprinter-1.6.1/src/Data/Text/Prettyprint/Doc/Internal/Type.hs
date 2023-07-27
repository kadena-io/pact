-- | __Internal module with stability guarantees__
--
-- This module exposes the internals of the @'Doc'@ type so other libraries can
-- write adaptors to/from it. For all other uses, please use only the API
-- provided by non-internal modules.
--
-- Although this module is internal, it follows the usual package versioning
-- policy, AKA Haskell’s version of semantic versioning. In other words, this
-- module is as stable as the public API.
module Data.Text.Prettyprint.Doc.Internal.Type (Doc(..)) where

import Data.Text.Prettyprint.Doc.Internal
