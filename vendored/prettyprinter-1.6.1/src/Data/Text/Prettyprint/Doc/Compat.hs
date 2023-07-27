{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module: Data.Text.Prettyprint.Doc.Compat
-- Copyright: Copyright © 2023 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- Translate between Doc values of the internal (vendored) prettyprinter
-- implementation and of recent versions (>=1.7) of prettyprinter package on
-- Hackage.
--
module Data.Text.Prettyprint.Doc.Compat
( docToInternal
, docFromInternal
) where

import "prettyprinter" Prettyprinter.Internal.Type qualified as PP
import "prettyprinter" Prettyprinter.Internal qualified as PP
import Data.Text.Prettyprint.Doc.Internal.Type
import Data.Text.Prettyprint.Doc.Internal

pageWidthFromInternal :: PageWidth -> PP.PageWidth
pageWidthFromInternal (AvailablePerLine a b) = PP.AvailablePerLine a b
pageWidthFromInternal Unbounded = PP.Unbounded

pageWidthToInternal :: PP.PageWidth -> PageWidth
pageWidthToInternal (PP.AvailablePerLine a b) = AvailablePerLine a b
pageWidthToInternal PP.Unbounded = Unbounded

docFromInternal :: Doc a -> PP.Doc a
docFromInternal Fail = PP.Fail
docFromInternal Empty = PP.Empty
docFromInternal (Char a) = PP.Char a
docFromInternal (Text a b) = PP.Text a b
docFromInternal Line = PP.Line
docFromInternal (FlatAlt a b) = PP.FlatAlt (docFromInternal a) (docFromInternal b)
docFromInternal (Cat a b) = PP.Cat (docFromInternal a) (docFromInternal b)
docFromInternal (Nest a b) = PP.Nest a (docFromInternal b)
docFromInternal (Union a b) = PP.Union (docFromInternal a) (docFromInternal b)
docFromInternal (Column a) = PP.Column $ docFromInternal <$> a
docFromInternal (WithPageWidth a) = PP.WithPageWidth (docFromInternal <$> a . pageWidthToInternal)
docFromInternal (Nesting a) = PP.Nesting (docFromInternal <$> a)
docFromInternal (Annotated a b) = PP.Annotated a (docFromInternal b)

docToInternal :: PP.Doc a -> Doc a
docToInternal PP.Fail = Fail
docToInternal PP.Empty = Empty
docToInternal (PP.Char a) = Char a
docToInternal (PP.Text a b) = Text a b
docToInternal PP.Line = Line
docToInternal (PP.FlatAlt a b) = FlatAlt (docToInternal a) (docToInternal b)
docToInternal (PP.Cat a b) = Cat (docToInternal a) (docToInternal b)
docToInternal (PP.Nest a b) = Nest a (docToInternal b)
docToInternal (PP.Union a b) = Union (docToInternal a) (docToInternal b)
docToInternal (PP.Column a) = Column (docToInternal <$> a)
docToInternal (PP.WithPageWidth a) = WithPageWidth (docToInternal <$> a . pageWidthFromInternal)
docToInternal (PP.Nesting a) = Nesting (docToInternal <$> a)
docToInternal (PP.Annotated a b) = Annotated a (docToInternal b)
