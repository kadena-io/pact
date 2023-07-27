-- | __Warning: internal module!__ This means that the API may change
-- arbitrarily between versions without notice. Depending on this module may
-- lead to unexpected breakages, so proceed with caution!
--
-- This module provides debugging helpers for inspecting 'Doc's.
--
-- Use the @pretty-simple@ package to get a nicer layout for 'show'n
-- 'Diag's:
--
-- > > Text.Pretty.Simple.pPrintNoColor . diag $ align (vcat ["foo", "bar"])
-- > Column
-- >    [
-- >        ( 10
-- >        , Nesting
-- >            [
-- >                ( 10
-- >                , Cat ( Text 3 "foo" )
-- >                    ( Cat ( FlatAlt Line Empty ) ( Text 3 "bar" ) )
-- >                )
-- >            ]
-- >        )
-- >    ]


module Data.Text.Prettyprint.Doc.Internal.Debug where

import Data.Text (Text)
import Data.Text.Prettyprint.Doc.Internal (PageWidth, Doc)
import qualified Data.Text.Prettyprint.Doc.Internal as Doc

-- | A variant of 'Doc' for debugging.
--
-- Unlike in the 'Doc' type, the 'Column', 'WithPageWidth' and 'Nesting'
-- constructors don't contain functions but are \"sampled\" to allow
-- simple inspection with 'show'.
data Diag ann =
    Fail
    | Empty
    | Char !Char
    | Text !Int !Text
    | Line
    | FlatAlt (Diag ann) (Diag ann)
    | Cat (Diag ann) (Diag ann)
    | Nest !Int (Diag ann)
    | Union (Diag ann) (Diag ann)
    | Column [(Int, Diag ann)]
      -- ^ 'Doc': @(Int -> Diag ann)@
    | WithPageWidth [(PageWidth, Diag ann)]
      -- ^ 'Doc': @(PageWidth -> Diag ann)@
    | Nesting [(Int, Diag ann)]
      -- ^ 'Doc': @(Int -> Diag ann)@
    | Annotated ann (Diag ann)
    deriving Show

-- | Convert a 'Doc' to its diagnostic representation.
--
-- The functions in the 'Column', 'WithPageWidth' and 'Nesting' constructors are
-- sampled with some default values.
--
-- Use `diag'` to control the function inputs yourself.
--
-- >>> diag $ Doc.align (Doc.vcat ["foo", "bar"])
-- Column [(10,Nesting [(10,Cat (Text 3 "foo") (Cat (FlatAlt Line Empty) (Text 3 "bar")))])]
diag :: Doc ann -> Diag ann
diag = diag' [10] [Doc.defaultPageWidth] [10]

diag'
    :: [Int]
       -- ^ Cursor positions for the 'Column' constructor
    -> [PageWidth]
       -- ^ For 'WithPageWidth'
    -> [Int]
       -- ^ Nesting levels for 'Nesting'
    -> Doc ann
    -> Diag ann
diag' columns pageWidths nestings = go
  where
    go doc = case doc of
        Doc.Fail -> Fail
        Doc.Empty -> Empty
        Doc.Char c -> Char c
        Doc.Text l t -> Text l t
        Doc.Line -> Line
        Doc.FlatAlt a b -> FlatAlt (go a) (go b)
        Doc.Cat a b -> Cat (go a) (go b)
        Doc.Nest i d -> Nest i (go d)
        Doc.Union a b -> Union (go a) (go b)
        Doc.Column f -> Column (apply f columns)
        Doc.WithPageWidth f -> WithPageWidth (apply f pageWidths)
        Doc.Nesting f -> Nesting (apply f nestings)
        Doc.Annotated ann d -> Annotated ann (go d)

    apply :: (a -> Doc ann) -> [a] -> [(a, Diag ann)]
    apply f = map (\x -> (x, go (f x)))

