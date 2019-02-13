{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Pact.Types.Pretty
  ( (<+>)
  , Annot(..)
  , Doc
  , Pretty(..)
  , RenderColor(..)
  , align
  , angles
  , annotate
  , braces
  , bracesSep
  , brackets
  , bracketsSep
  , colon
  , commaBraces
  , commaBrackets
  , dot
  , dquotes
  , encloseSep
  , equals
  , fromAnsiWlPprint
  , hardline
  , hsep
  , indent
  , line
  , list
  , nest
  , parens
  , parensSep
  , punctuate
  , putDoc
  , renderCompactString
  , renderCompactString'
  , renderCompactText
  , renderCompactText'
  , renderPrettyString
  , renderPrettyString'
  , renderString'
  , sep
  , space
  , unAnnotate
  , viaShow
  , vsep
  ) where

import           Bound.Var
import           Data.Aeson           (Value(..))
import qualified Data.ByteString.UTF8 as UTF8
import           Data.Foldable        (toList)
import qualified Data.HashMap.Strict  as HM
import           Data.Int
import           Data.Text            (Text, unpack)
import           Data.Text.Prettyprint.Convert.AnsiWlPprint (fromAnsiWlPprint)
import           Data.Text.Prettyprint.Doc
  (SimpleDocStream, annotate, unAnnotate, layoutCompact, layoutPretty,
  defaultLayoutOptions, vsep, hsep, (<+>), colon, angles, list, braces,
  brackets, encloseSep, parens, sep, line, dquotes, viaShow, punctuate, dot,
  encloseSep, space, nest, align, hardline, tupled, indent, equals, reAnnotate,
  reAnnotateS)
import qualified Data.Text.Prettyprint.Doc as PP
import qualified Data.Text.Prettyprint.Doc.Render.String as PP
import           Data.Text.Prettyprint.Doc.Render.Terminal
  (color, Color(..), AnsiStyle)
import qualified Data.Text.Prettyprint.Doc.Render.Terminal as Term
import           Data.Text.Prettyprint.Doc.Render.Text as RText
import           Text.Trifecta.Delta

data RenderColor = RColor | RPlain

-- | Annotations allowed on pretty-printing 'Doc's
data Annot
  = Warning
  | Err
  | Header
  | Val
  | Example
  | BadExample

type Doc = PP.Doc Annot

-- | Pact's version of 'Pretty', with 'Annot' annotations.
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

commaBraces, commaBrackets, bracketsSep, parensSep, bracesSep :: [Doc] -> Doc
commaBraces   = encloseSep "{" "}" ","
commaBrackets = encloseSep "[" "]" ","
bracketsSep   = brackets . sep
parensSep     = parens . sep
bracesSep     = braces . sep

renderString'
  :: (Doc -> SimpleDocStream Annot) -> RenderColor -> Doc -> String
renderString' renderf colors doc = case colors of
  RColor -> unpack $ Term.renderStrict $ reAnnotateS colorFun $ renderf doc
  RPlain -> PP.renderString $ renderf $ unAnnotate doc

colorFun :: Annot -> AnsiStyle
colorFun = color . \case
  Warning    -> Yellow
  Err        -> Red
  Header     -> Green
  Val        -> Blue
  Example    -> Green
  BadExample -> Red

renderCompactString :: Pretty a => a -> String
renderCompactString = renderString' layoutCompact RPlain . pretty

renderCompactString' :: Doc -> String
renderCompactString' = renderString' layoutCompact RPlain

renderCompactText :: Pretty a => a -> Text
renderCompactText
  = RText.renderStrict . layoutCompact . reAnnotate colorFun . pretty

renderCompactText' :: PP.Doc Annot -> Text
renderCompactText' = Term.renderStrict . layoutCompact . reAnnotate colorFun

renderPrettyString :: Pretty a => RenderColor -> a -> String
renderPrettyString rc
  = renderString' (layoutPretty defaultLayoutOptions) rc . pretty

renderPrettyString' :: RenderColor -> Doc -> String
renderPrettyString' = renderString' $ layoutPretty defaultLayoutOptions

instance Pretty Value where
  pretty = \case
    Object hm -> commaBraces
      $ fmap (\(k, v) -> dquotes (pretty k) <> ": " <> pretty v)
      $ HM.toList hm
    Array values -> bracketsSep $ pretty <$> toList values
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
