{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}

#include "version-compatibility-macros.h"

-- | This module shows how to write a custom prettyprinter backend, based on a
-- tree representation of a 'SimpleDocStream'.  For a stack machine approach, which
-- may be more suitable for certain output formats, see
-- "Data.Text.Prettyprint.Doc.Render.Tutorials.StackMachineTutorial".
--
-- Rendering to HTML, particularly using libraries such as blaze-html or lucid,
-- is one important use case of tree-based rendering.
--
-- The module is written to be readable top-to-bottom in both Haddock and raw
-- source form.
module Data.Text.Prettyprint.Doc.Render.Tutorials.TreeRenderingTutorial where

import qualified Data.Text.Lazy         as TL
import qualified Data.Text.Lazy.Builder as TLB

import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Internal
import Data.Text.Prettyprint.Doc.Render.Util.SimpleDocTree

#if !(FOLDABLE_TRAVERSABLE_IN_PRELUDE)
import Data.Foldable (foldMap)
#endif
#if !(SEMIGROUP_MONOID_SUPERCLASS)
import Data.Semigroup
#endif

-- * The type of available markup
--
-- $standalone-text
--
-- First, we define a set of valid annotations must be defined, with the goal of
-- defining a @'Doc' 'SimpleHtml'@. We will later define how to convert this to
-- the output format ('TL.Text').

data SimpleHtml = Bold | Italics | Color Color | Paragraph | Headline
data Color = Red | Green | Blue

-- ** Convenience definitions

bold, italics, paragraph, headline :: Doc SimpleHtml -> Doc SimpleHtml
bold = annotate Bold
italics = annotate Italics
paragraph = annotate Paragraph
headline = annotate Headline

color :: Color -> Doc SimpleHtml -> Doc SimpleHtml
color c = annotate (Color c)

-- * The rendering algorithm
--
-- $standalone-text
--
-- With the annotation definitions out of the way, we can now define a
-- conversion function from 'SimpleDocStream' (annotated with our 'SimpleHtml')
-- to the tree-shaped 'SimpleDocTree', which is easily convertible to a
-- HTML/'Text' representation.
--
-- There are two ways to render this; the simpler one is just using
-- 'renderSimplyDecorated'. However, some output formats require more
-- complicated functionality, so we explore this explicitly with a simple
-- example below. An example for something more complicated is e.g. an XHTML
-- renderer, where a newline may not simply be a newline character followed by a
-- certain number of spaces, but e.g. involve adding a @<br/>@ tag.

-- | To render the HTML, we first convert the 'SimpleDocStream' to the
-- 'SimpleDocTree' format, which makes enveloping sub-documents in markup
-- easier.
--
-- This function is the entry main API function of the renderer; as such, it is
-- only glue for the internal functions. This is similar to
-- 'Data.Text.Prettyprint.Doc.Render.Tutorials.StackMachineTutorial.render' from
-- the stack machine tutorial in its purpose.
render :: SimpleDocStream SimpleHtml -> TL.Text
render = TLB.toLazyText . renderTree . treeForm

-- | Render a 'SimpleDocTree' to a 'TLB.Builder'; this is the workhorse of the
-- tree-based rendering approach, and equivalent to
-- 'Data.Text.Prettyprint.Doc.Render.Tutorials.StackMachineTutorial.renderStackMachine'
-- in the stack machine rendering tutorial.
renderTree :: SimpleDocTree SimpleHtml -> TLB.Builder
renderTree sds = case sds of
    STEmpty -> mempty
    STChar c -> TLB.singleton c
    STText _ t -> TLB.fromText t
    STLine i -> "\n" <> TLB.fromText (textSpaces i)
    STAnn ann content -> encloseInTagFor ann (renderTree content)
    STConcat contents -> foldMap renderTree contents

-- | Convert a 'SimpleHtml' to a function that encloses a 'TLB.Builder' in HTML
-- tags. This is where the translation of style to raw output happens.
encloseInTagFor :: SimpleHtml -> TLB.Builder -> TLB.Builder
encloseInTagFor sh = case sh of
    Bold      -> \x -> "<strong>" <> x <> "</strong>"
    Italics   -> \x -> "<em>" <> x <> "</em>"
    Color c   -> \x -> "<span style=\"color: " <> hexCode c <> "\">" <> x <> "</span>"
    Paragraph -> \x -> "<p>" <> x <> "</p>"
    Headline  -> \x -> "<h1>" <> x <> "</h1>"
  where
    hexCode :: Color -> TLB.Builder
    hexCode c = case c of
        Red   -> "#f00"
        Green -> "#0f0"
        Blue  -> "#00f"

-- * Example invocation
--
-- $standalone-text
--
-- We can now render an example document using our definitions:
--
-- >>> :set -XOverloadedStrings
-- >>> import qualified Data.Text.Lazy.IO as TL
-- >>> :{
-- >>> let go = TL.putStrLn . render . layoutPretty defaultLayoutOptions
-- >>> in go (vsep
-- >>>     [ headline "Example document"
-- >>>     , paragraph ("This is a" <+> color Red "paragraph" <> comma)
-- >>>     , paragraph ("and" <+> bold "this text is bold.")
-- >>>     ])
-- >>> :}
-- <h1>Example document</h1>
-- <p>This is a <span style="color: #f00">paragraph</span>,</p>
-- <p>and <strong>this text is bold.</strong></p>
