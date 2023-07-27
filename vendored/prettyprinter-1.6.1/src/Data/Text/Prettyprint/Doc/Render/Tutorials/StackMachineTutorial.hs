{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -fno-warn-deprecations #-}

#include "version-compatibility-macros.h"

-- | This module shows how to write a custom prettyprinter backend, based on
-- directly converting a 'SimpleDocStream' to an output format using a stack
-- machine. For a tree serialization approach, which may be more suitable for
-- certain output formats, see
-- "Data.Text.Prettyprint.Doc.Render.Tutorials.TreeRenderingTutorial".
--
-- Rendering to ANSI terminal with colors is an important use case for stack
-- machine based rendering.
--
-- The module is written to be readable top-to-bottom in both Haddock and raw
-- source form.
module Data.Text.Prettyprint.Doc.Render.Tutorials.StackMachineTutorial
    {-# DEPRECATED "Writing your own stack machine is probably more efficient and customizable; also consider using »renderSimplyDecorated(A)« instead" #-}
    where

import qualified Data.Text.Lazy         as TL
import qualified Data.Text.Lazy.Builder as TLB

import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Internal
import Data.Text.Prettyprint.Doc.Render.Util.Panic
import Data.Text.Prettyprint.Doc.Render.Util.StackMachine

#if !(APPLICATIVE_MONAD)
import Control.Applicative
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
-- conversion function from 'SimpleDocStream' annotated with our 'SimpleHtml' to the
-- final 'TL.Text' representation.
--
-- There are two ways to render this; the simpler one is just using
-- 'renderSimplyDecorated'. However, some output formats require more
-- complicated functionality, so we explore this explicitly with a simple
-- example below. An example for something more complicated is ANSI terminal
-- rendering, where on popping we need to regenerate the previous style,
-- requiring a pop (discard current style) followed by a peek (regenerate
-- previous style).

-- | The 'StackMachine' type defines a stack machine suitable for many rendering
-- needs. It has two auxiliary parameters: the type of the end result, and the
-- type of the document’s annotations.
--
-- Most 'StackMachine' creations will look like this definition: a recursive
-- walk through the 'SimpleDocStream', pushing styles on the stack and popping
-- them off again, and writing raw output.
--
-- The equivalent to this in the tree based rendering approach is
-- 'Data.Text.Prettyprint.Doc.Render.Tutorials.TreeRenderingTutorial.renderTree'.
renderStackMachine :: SimpleDocStream SimpleHtml -> StackMachine TLB.Builder SimpleHtml ()
renderStackMachine = \sds -> case sds of
    SFail -> panicUncaughtFail
    SEmpty -> pure ()
    SChar c x -> do
        writeOutput (TLB.singleton c)
        renderStackMachine x
    SText _l t x -> do
        writeOutput (TLB.fromText t)
        renderStackMachine x
    SLine i x -> do
        writeOutput (TLB.singleton '\n')
        writeOutput (TLB.fromText (textSpaces i))
        renderStackMachine x
    SAnnPush s x -> do
        pushStyle s
        writeOutput (fst (htmlTag s))
        renderStackMachine x
    SAnnPop x -> do
        s <- unsafePopStyle
        writeOutput (snd (htmlTag s))
        renderStackMachine x

-- | Convert a 'SimpleHtml' annotation to a pair of opening and closing tags.
-- This is where the translation of style to raw output happens.
htmlTag :: SimpleHtml -> (TLB.Builder, TLB.Builder)
htmlTag = \sh -> case sh of
    Bold      -> ("<strong>", "</strong>")
    Italics   -> ("<em>", "</em>")
    Color c   -> ("<span style=\"color: " <> hexCode c <> "\">", "</span>")
    Paragraph -> ("<p>", "</p>")
    Headline  -> ("<h1>", "</h1>")
  where
    hexCode :: Color -> TLB.Builder
    hexCode = \c -> case c of
        Red   -> "#f00"
        Green -> "#0f0"
        Blue  -> "#00f"

-- | We can now wrap our stack machine definition from 'renderStackMachine' in a
-- nicer interface; on successful conversion, we run the builder to give us the
-- final 'TL.Text', and before we do that we check that the style stack is empty
-- (i.e. there are no unmatched style applications) after the machine is run.
--
-- This function does only a bit of plumbing around 'renderStackMachine', and is
-- the main API function of a stack machine renderer. The tree renderer
-- equivalent to this is
-- 'Data.Text.Prettyprint.Doc.Render.Tutorials.TreeRenderingTutorial.render'.
render :: SimpleDocStream SimpleHtml -> TL.Text
render doc
  = let (resultBuilder, remainingStyles) = execStackMachine [] (renderStackMachine doc)
    in if null remainingStyles
        then TLB.toLazyText resultBuilder
        else error ("There are "
                    <> show (length remainingStyles)
                    <> " unpaired styles! Please report this as a bug.")

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
