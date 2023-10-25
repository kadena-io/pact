-- |
-- Module      :  Data.Text.Prettyprint.Doc
-- Copyright   :  Daan Leijen (c) 2000, http://www.cs.uu.nl/~daan
--                Max Bolingbroke (c) 2008, http://blog.omega-prime.co.uk
--                David Luposchainsky (c) 2016, http://github.com/quchen
-- License     :  BSD-style (see the file LICENSE.md)
-- Maintainer  :  David Luposchainsky <dluposchainsky (λ) google>
-- Stability   :  experimental
-- Portability :  portable
--
-- = Overview
--
-- This module defines a prettyprinter to format text in a flexible and
-- convenient way. The idea is to combine a 'Doc'ument out of many small
-- components, then using a layouter to convert it to an easily renderable
-- 'SimpleDocStream', which can then be rendered to a variety of formats, for
-- example plain 'Text'.
--
-- The documentation consists of several parts:
--
--   1. Just below is some general information about the library.
--   2. The actual library with extensive documentation and examples
--   3. Migration guide for users familiar with (ansi-)wl-pprint
--
-- == Starting out
--
-- As a reading list for starters, some of the most commonly used functions in
-- this module include '<>', 'hsep', '<+>', 'vsep', 'align', 'hang'. These cover
-- many use cases already, and many other functions are variations or
-- combinations of these.
--
-- = Simple example
--
-- Let’s prettyprint a simple Haskell type definition. First, intersperse @->@
-- and add a leading @::@,
--
-- >>> let prettyType = align . sep . zipWith (<+>) ("::" : repeat "->")
--
-- The 'sep' function is one way of concatenating documents, there are multiple
-- others, e.g. 'vsep', 'cat' and 'fillSep'. In our case, 'sep' space-separates
-- all entries if there is space, and newlines if the remaining line is too
-- short.
--
-- Second, prepend the name to the type,
--
-- >>> let prettyDecl n tys = pretty n <+> prettyType tys
--
-- Now we can define a document that contains some type signature:
--
-- >>> let doc = prettyDecl "example" ["Int", "Bool", "Char", "IO ()"]
--
-- This document can now be printed, and it automatically adapts to available
-- space. If the page is wide enough (80 characters in this case), the
-- definitions are space-separated,
--
-- >>> putDocW 80 doc
-- example :: Int -> Bool -> Char -> IO ()
--
-- If we narrow the page width to only 20 characters, the /same document/
-- renders vertically aligned:
--
-- >>> putDocW 20 doc
-- example :: Int
--         -> Bool
--         -> Char
--         -> IO ()
--
-- Speaking of alignment, had we not used 'align', the @->@ would be at the
-- beginning of each line, and not beneath the @::@.
--
-- The 'Data.Text.Prettyprint.Doc.Util.putDocW' renderer used here is from
-- "Data.Text.Prettyprint.Doc.Util".
--
-- = General workflow
--
-- @
-- ╔══════════╗
-- ║          ║                         ╭────────────────────╮
-- ║          ║                         │ 'vsep', 'pretty', '<+>', │
-- ║          ║                         │ 'nest', 'align', …     │
-- ║          ║                         ╰─────────┬──────────╯
-- ║          ║                                   │
-- ║  Create  ║                                   │
-- ║          ║                                   │
-- ║          ║                                   ▽
-- ║          ║                         ╭───────────────────╮
-- ║          ║                         │        'Doc'        │
-- ╠══════════╣                         │  (rich document)  │
-- ║          ║                         ╰─────────┬─────────╯
-- ║          ║                                   │
-- ║          ║                                   │ Layout algorithms
-- ║  Layout  ║                                   │ e.g. 'layoutPretty'
-- ║          ║                                   ▽
-- ║          ║                         ╭───────────────────╮
-- ║          ║                         │  'SimpleDocStream'  │
-- ╠══════════╣                         │ (simple document) │
-- ║          ║                         ╰─────────┬─────────╯
-- ║          ║                                   │
-- ║          ║                                   ├─────────────────────────────╮
-- ║          ║                                   │                             │ 'Data.Text.Prettyprint.Doc.Render.Util.SimpleDocTree.treeForm'
-- ║          ║                                   │                             ▽
-- ║          ║                                   │                     ╭───────────────╮
-- ║          ║                                   │                     │ 'Data.Text.Prettyprint.Doc.Render.Util.SimpleDocTree.SimpleDocTree' │
-- ║  Render  ║                                   │                     ╰───────┬───────╯
-- ║          ║                                   │                             │
-- ║          ║               ╭───────────────────┼─────────────────╮  ╭────────┴────────╮
-- ║          ║               │                   │                 │  │                 │
-- ║          ║               ▽                   ▽                 ▽  ▽                 ▽
-- ║          ║       ╭───────────────╮   ╭───────────────╮   ╭───────────────╮   ╭───────────────╮
-- ║          ║       │ ANSI terminal │   │  Plain 'Text'   │   │ other/custom  │   │     HTML      │
-- ║          ║       ╰───────────────╯   ╰───────────────╯   ╰───────────────╯   ╰───────────────╯
-- ║          ║
-- ╚══════════╝
-- @
--
-- = How the layout works
--
-- There are two key concepts to laying a document out: the available width, and
-- 'group'ing.
--
-- == Available width
--
-- The page has a certain maximum width, which the layouter tries to not exceed,
-- by inserting line breaks where possible. The functions given in this module
-- make it fairly straightforward to specify where, and under what
-- circumstances, such a line break may be inserted by the layouter, for example
-- via the 'sep' function.
--
-- There is also the concept of /ribbon width/. The ribbon is the part of a line
-- that is printed, i.e. the line length without the leading indentation. The
-- layouters take a ribbon fraction argument, which specifies how much of a line
-- should be filled before trying to break it up. A ribbon width of 0.5 in a
-- document of width 80 will result in the layouter to try to not exceed @0.5*80 =
-- 40@ (ignoring current indentation depth).
--
-- == Grouping
--
-- A document can be 'group'ed, which tells the layouter that it should attempt
-- to collapse it to a single line. If the result does not fit within the
-- constraints (given by page and ribbon widths), the document is rendered
-- unaltered. This allows fallback definitions, so that we get nice results even
-- when the original document would exceed the layout constraints.
--
-- = Things the prettyprinter /cannot/ do
--
-- Due to how the Wadler/Leijen algorithm is designed, a couple of things are
-- unsupported right now, with a high possibility of having no sensible
-- implementation without significantly changing the layout algorithm. In
-- particular, this includes
--
--   * Leading symbols instead of just spaces for indentation, as used by the
--     Linux @tree@ tool for example
--   * Multi-column layouts, in particular tables with multiple cells of equal
--     width adjacent to each other
--
-- = Some helpful tips
--
-- == Which kind of annotation should I use?
--
-- __Summary:__ Use semantic annotations for @'Doc'@, and after layouting map to
-- backend-specific ones.
--
-- For example, suppose you want to prettyprint some programming language code.
-- If you want keywords to be red, you should annotate the @'Doc'@ with a type
-- that has a 'Keyword' field (without any notion of color), and then after
-- layouting convert the annotations to map @'Keyword'@ to e.g. @'Red'@ (using
-- @'reAnnotateS'@). The alternative that I /do not/ recommend is directly
-- annotating the @'Doc'@ with 'Red'.
--
-- While both versions would superficially work equally well and would create
-- identical output, the recommended way has two significant advantages:
-- modularity and extensibility.
--
-- /Modularity:/ To change the color of keywords later, you have to touch one
-- point, namely the mapping in @'reAnnotateS'@, where @'Keyword'@ is mapped to
-- 'Red'. If you have @'annotate Red …'@ everywher, you’ll have to do a full
-- text replacement, producing a large diff and touching lots of places for a
-- very small change.
--
-- /Extensibility:/ Adding a different backend in the recommended version is
-- simply adding another @'reAnnotateS'@ to convert the @'Doc'@ annotation to
-- something else. On the other hand, if you have @'Red'@ as an annotation in
-- the @'Doc'@ already and the other backend does not support anything red
-- (think of plain text or a website where red doesn’t work well with the rest
-- of the style), you’ll have to worry about what to map »redness« to, which has
-- no canonical answer. Should it be omitted? What does »red« mean anyway –
-- maybe keywords and variables are red, and you want to change only the color
-- of variables?
module Data.Text.Prettyprint.Doc (
    -- * Documents
    Doc,

    -- * Basic functionality
    Pretty(..),
    viaShow, unsafeViaShow,
    emptyDoc, nest, line, line', softline, softline', hardline, group, flatAlt,

    -- * Alignment functions
    --
    -- | The functions in this section cannot be described by Wadler's original
    -- functions. They align their output relative to the current output
    -- position - in contrast to @'nest'@ which always aligns to the current
    -- nesting level. This deprives these functions from being \'optimal\'. In
    -- practice however they prove to be very useful. The functions in this
    -- section should be used with care, since they are more expensive than the
    -- other functions. For example, @'align'@ shouldn't be used to pretty print
    -- all top-level declarations of a language, but using @'hang'@ for let
    -- expressions is fine.
    align, hang, indent, encloseSep, list, tupled,

    -- * Binary functions
    (<>), (<+>),

    -- * List functions

    -- | The 'sep' and 'cat' functions differ in one detail: when 'group'ed, the
    -- 'sep's replace newlines wich 'space's, while the 'cat's simply remove
    -- them. If you're not sure what you want, start with the 'sep's.

    concatWith,

    -- ** 'sep' family
    --
    -- | When 'group'ed, these will replace newlines with spaces.
    hsep, vsep, fillSep, sep,
    -- ** 'cat' family
    --
    -- | When 'group'ed, these will remove newlines.
    hcat, vcat, fillCat, cat,
    -- ** Others
    punctuate,

    -- * Reactive/conditional layouts
    --
    -- | Lay documents out differently based on current position and the page
    -- layout.
    column, nesting, width, pageWidth,

    -- * Filler functions
    --
    -- | Fill up available space
    fill, fillBreak,

    -- * General convenience
    --
    -- | Useful helper functions.
    plural, enclose, surround,

    -- * Bracketing functions
    --
    -- | Enclose documents in common ways.
    squotes, dquotes, parens, angles, brackets, braces,

    -- * Named characters
    --
    -- | Convenience definitions for common characters
    lparen, rparen, langle, rangle, lbrace, rbrace, lbracket, rbracket, squote,
    dquote, semi, colon, comma, space, dot, slash, backslash, equals, pipe,

    -- ** Annotations
    annotate,
    unAnnotate,
    reAnnotate,
    alterAnnotations,
    unAnnotateS,
    reAnnotateS,
    alterAnnotationsS,

    -- * Optimization
    --
    -- Render documents faster
    fuse, FusionDepth(..),

    -- * Layout
    --
    -- | Laying a 'Doc'ument out produces a straightforward 'SimpleDocStream'
    -- based on parameters such as page width and ribbon size, by evaluating how
    -- a 'Doc' fits these constraints the best. There are various ways to render
    -- a 'SimpleDocStream'. For the common case of rendering a 'SimpleDocStream'
    -- as plain 'Text' take a look at "Data.Text.Prettyprint.Doc.Render.Text".
    SimpleDocStream(..),
    PageWidth(..), LayoutOptions(..), defaultLayoutOptions,
    layoutPretty, layoutCompact, layoutSmart,
    removeTrailingWhitespace,

    -- * Migration guide
    --
    -- $migration
) where



import Data.Text.Prettyprint.Doc.Internal
import Data.Text.Prettyprint.Doc.Symbols.Ascii

-- $setup
--
-- (Definitions for the doctests)
--
-- >>> :set -XOverloadedStrings
-- >>> import Data.Text.Prettyprint.Doc.Render.Text
-- >>> import Data.Text.Prettyprint.Doc.Util



-- $migration
--
-- There are 3 main ways to migrate:
--
--   1. Direct: just replace the previous package and fix the errors
--   2. Using a drop-in replacement mimicing the API of the former module, see
--      the @prettyprinter-compat-<former package>@ packages
--   3. Using a converter from the old @Doc@ type to the new one, see the
--      @prettyprinter-convert-<former package>@ packages
--
-- If you're already familiar with (ansi-)wl-pprint, you'll recognize many
-- functions in this module, and they work just the same way. However, a couple
-- of definitions are missing:
--
--   - @char@, @string@, @double@, … – these are all special cases of the
--     overloaded @'pretty'@ function.
--   - @\<$>@, @\<$$>@, @\</>@, @\<//>@ are special cases of
--     @'vsep'@, @'vcat'@, @'fillSep'@, @'fillCat'@ with only two documents.
--   - If you need 'String' output, use the backends in the
--     "Data.Text.Prettyprint.Doc.Render.String" module.
--   - The /display/ functions are moved to the rendering submodules, for
--     example conversion to plain 'Text' is in the
--     "Data.Text.Prettyprint.Doc.Render.Text" module.
--   - The /render/ functions are called /layout/ functions.
--   - @SimpleDoc@ was renamed to @'SimpleDocStream'@, in order to make it
--     clearer in the presence of @SimpleDocTree@.
--   - Instead of providing an own colorization function for each
--     color\/intensity\/layer combination, they have been combined in 'color',
--     'colorDull', 'bgColor', and 'bgColorDull' functions, which can be found
--     in the ANSI terminal specific @prettyprinter-ansi-terminal@ package.
