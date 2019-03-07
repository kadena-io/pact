{-# LANGUAGE BangPatterns #-}
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
  , abbrev
  , abbrevStr
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
  , fillSep
  , fromAnsiWlPprint
  , hardline
  , hsep
  , indent
  , line
  , list
  , nest
  , parens
  , parensSep
  , prettyString
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
import           Data.Text            (Text, pack, unpack)
import qualified Data.Text            as Text
import           Data.Text.Prettyprint.Convert.AnsiWlPprint (fromAnsiWlPprint)
import           Data.Text.Prettyprint.Doc
  (SimpleDocStream, annotate, unAnnotate, layoutPretty,
  defaultLayoutOptions, vsep, hsep, (<+>), colon, angles, list, braces,
  brackets, encloseSep, parens, sep, line, dquotes, viaShow, punctuate, dot,
  encloseSep, space, nest, align, hardline, tupled, indent, equals, reAnnotate,
  reAnnotateS, fillSep)
import qualified Data.Text.Prettyprint.Doc as PP
import qualified Data.Text.Prettyprint.Doc.Internal.Type as PP
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
instance Pretty () where pretty = PP.pretty
instance (Pretty a, Pretty b) => Pretty (a, b) where
  pretty (a, b) = tupled [pretty a, pretty b]
instance (Pretty a, Pretty b, Pretty c) => Pretty (a, b, c) where
  pretty (a, b, c) = tupled [pretty a, pretty b, pretty c]
instance Pretty a => Pretty (Maybe a) where pretty = maybe mempty pretty

prettyString :: String -> Doc
prettyString = PP.pretty . pack

commaBraces, commaBrackets, bracketsSep, parensSep, bracesSep :: [Doc] -> Doc
commaBraces   = encloseSep "{" "}" ","
commaBrackets = encloseSep "[" "]" ","
bracketsSep   = brackets . sep
parensSep     = parens   . sep
bracesSep     = braces   . sep

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

-- | The same as @layoutCompact@, except this printer never inserts a line
-- break if it's given the choice (at @FlatAlt@).
layoutReallyCompact :: PP.Doc ann -> SimpleDocStream ann
layoutReallyCompact doc = scan 0 [doc]
  where
    scan _ [] = PP.SEmpty
    scan !col (d:ds) = case d of
        PP.Fail            -> PP.SFail
        PP.Empty           -> scan col ds
        PP.Char c          -> PP.SChar c (scan (col+1) ds)
        PP.Text l t        -> let !col' = col+l in PP.SText l t (scan col' ds)
        PP.FlatAlt _ y     -> scan col (y:ds)
        PP.Line            -> PP.SLine 0 (scan 0 ds)
        PP.Cat x y         -> scan col (x:y:ds)
        PP.Nest _ x        -> scan col (x:ds)
        PP.Union _ y       -> scan col (y:ds)
        PP.Column f        -> scan col (f col:ds)
        PP.WithPageWidth f -> scan col (f PP.Unbounded : ds)
        PP.Nesting f       -> scan col (f 0 : ds)
        PP.Annotated _ x   -> scan col (x:ds)

renderCompactString :: Pretty a => a -> String
renderCompactString = renderString' layoutReallyCompact RPlain . pretty

renderCompactString' :: Doc -> String
renderCompactString' = renderString' layoutReallyCompact RPlain

renderCompactText :: Pretty a => a -> Text
renderCompactText
  = RText.renderStrict . layoutReallyCompact . reAnnotate colorFun . pretty

renderCompactText' :: PP.Doc Annot -> Text
renderCompactText' = Term.renderStrict . layoutReallyCompact . reAnnotate colorFun

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
    Number scientific -> viaShow scientific
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
          = prettyString source
            <> pretty ':' <> pretty (line'+1)
            <> pretty ':' <> pretty (column'+1)
        interactive = "(interactive)"

instance (Pretty a, Pretty b) => Pretty (Var a b) where
  pretty = \case
    B b -> pretty b
    F a -> pretty a

-- | Abbreviated 'pretty'
abbrev :: Pretty a => a -> Text
abbrev a =
  let fullText = renderCompactText a
  in case Text.compareLength fullText 50 of
       GT -> Text.take 50 fullText <> "..."
       _  -> fullText

abbrevStr :: Pretty a => a -> String
abbrevStr = unpack . abbrev
