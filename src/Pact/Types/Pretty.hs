{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Pact.Types.Pretty
  ( RenderColor(..)
  , Annot(..)
  , Doc
  , Pretty(..)
  , commaBraces
  , commaBrackets
  , spaceBrackets
  , parensSep
  , bracesSep
  , annotate
  , renderString'
  , renderCompactString
  , renderCompactString'
  , renderCompactText
  , renderCompactText'
  , renderPrettyString
  , renderPrettyString'
  , (<+>)
  , colon
  , hsep
  , angles
  , braces
  , brackets
  , line
  , list
  , dquotes
  , viaShow
  , punctuate
  , dot
  , encloseSep
  , space
  , nest
  , align
  , vsep
  , sep
  , fromAnsiWlPprint
  , putDoc
  , hardline
  , parens
  , indent
  , equals
  , unAnnotate
  ) where

import Bound.Var
import Data.Aeson (Value(..))
import qualified Data.HashMap.Strict as HM
import Data.Text (Text)
import           Data.Text.Prettyprint.Doc (SimpleDocStream, annotate, unAnnotate, layoutCompact, layoutPretty, defaultLayoutOptions, vsep, hsep, (<+>), colon, angles, list, braces, brackets, encloseSep, parens, sep, line, dquotes, viaShow, punctuate, dot, encloseSep, space, nest, align, hardline, tupled, indent, equals)
import qualified Data.Text.Prettyprint.Doc as PP
import Data.Text.Prettyprint.Doc.Render.Text as RText
import Data.Text.Prettyprint.Doc.Render.String (renderShowS)
import Data.Foldable (toList)
import Data.Int
import qualified Data.ByteString.UTF8 as UTF8
import Text.Trifecta.Delta
import Data.Text.Prettyprint.Convert.AnsiWlPprint (fromAnsiWlPprint)

data RenderColor = RColor | RPlain

data Annot
  = Warning
  | Err
  | Header
  | Val
  | Example
  | BadExample

type Doc = PP.Doc Annot

class Pretty a where
  pretty :: a -> Doc

instance Pretty a => Pretty [a] where
  pretty = list . map pretty

instance Pretty Text where pretty = PP.pretty
instance Pretty Char where pretty = PP.pretty
instance Pretty Bool where pretty = PP.pretty
instance Pretty Integer where pretty = PP.pretty
instance Pretty Int where pretty = PP.pretty
instance Pretty Int64 where pretty = viaShow
instance (Pretty a, Pretty b) => Pretty (a, b) where
  pretty (a, b) = tupled [pretty a, pretty b]
instance (Pretty a, Pretty b, Pretty c) => Pretty (a, b, c) where
  pretty (a, b, c) = tupled [pretty a, pretty b, pretty c]
instance Pretty a => Pretty (Maybe a) where pretty = maybe mempty pretty

commaBraces, commaBrackets, spaceBrackets, parensSep, bracesSep :: [Doc] -> Doc
commaBraces   = encloseSep "{" "}" ","
commaBrackets = encloseSep "[" "]" ","
spaceBrackets = encloseSep "[" "]" " "
parensSep     = parens . sep
bracesSep     = braces . sep

renderString'
  :: (Doc -> SimpleDocStream Annot) -> RenderColor -> Doc -> String
renderString' renderf colors doc =
  let display = case colors of
        RColor -> renderShowS . renderf
        RPlain -> renderShowS . renderf . unAnnotate
  in display doc ""

renderCompactString :: Pretty a => a -> String
renderCompactString = renderString' layoutCompact RPlain . pretty

renderCompactString' :: Doc -> String
renderCompactString' = renderString' layoutCompact RPlain

renderCompactText :: Pretty a => a -> Text
renderCompactText = RText.renderStrict . layoutCompact . pretty

renderCompactText' :: PP.Doc annot -> Text
renderCompactText' = RText.renderStrict . layoutCompact

renderPrettyString :: Pretty a => RenderColor -> a -> String
renderPrettyString rc a
  = renderString' (layoutPretty defaultLayoutOptions) rc $ pretty a

renderPrettyString' :: RenderColor -> Doc -> String
renderPrettyString' rc doc
  = renderString' (layoutPretty defaultLayoutOptions) rc doc

instance Pretty Value where
  pretty = \case
    Object hm -> commaBraces
      $ fmap (\(k, v) -> dquotes (pretty k) <> ": " <> pretty v)
      $ HM.toList hm
    Array values -> spaceBrackets $ pretty <$> toList values
    String str -> dquotes $ pretty str
    Number scientific -> pretty $ show scientific
    Bool b -> pretty b
    Null -> "null"

-- | Example: @file.txt:12:34@
instance Pretty Delta where
    pretty d = case d of
        Columns c _         -> prettyDelta interactive 0 c
        Tab x y _           -> prettyDelta interactive 0 (nextTab x + y)
        Lines l c _ _       -> prettyDelta interactive l c
        Directed fn l c _ _ -> prettyDelta (UTF8.toString fn) l c
      where
        prettyDelta
            :: String -- Source description
            -> Int64  -- Line
            -> Int64  -- Column
            -> Doc
        prettyDelta source line' column'
          = pretty source
            <> pretty ':' <> pretty (line'+1)
            <> pretty ':' <> pretty (column'+1)
        interactive = "(interactive)"

instance (Pretty a, Pretty b) => Pretty (Var a b) where
  pretty = \case
    B b -> pretty b
    F a -> pretty a
