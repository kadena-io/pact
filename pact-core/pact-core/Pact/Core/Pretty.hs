{-# LANGUAGE OverloadedStrings #-}

module Pact.Core.Pretty
( module Pretty
, renderCompactString
, renderCompactString'
, prettyCommaSep
, prettyCommaSepNE
) where

import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc as Pretty
import Data.Text.Prettyprint.Doc.Render.String
import Data.List(intersperse)
import Data.List.NonEmpty(NonEmpty)
import Data.Foldable(fold)

import qualified Data.List.NonEmpty as NE

renderCompactString :: Pretty a => a -> String
renderCompactString = renderString . layoutPretty defaultLayoutOptions . pretty

renderCompactString' :: Doc ann -> String
renderCompactString' = renderString . layoutPretty defaultLayoutOptions

prettyCommaSepNE :: Pretty a => NonEmpty a -> Doc ann
prettyCommaSepNE = fold . NE.intersperse ", " . fmap pretty

prettyCommaSep :: Pretty a => [a] -> Doc ann
prettyCommaSep = fold . intersperse ", " . fmap pretty
