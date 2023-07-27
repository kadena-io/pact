{-# LANGUAGE CPP                #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}

#include "version-compatibility-macros.h"

-- | Conversion of the linked-list-like 'SimpleDocStream' to a tree-like
-- 'SimpleDocTree'.
module Data.Text.Prettyprint.Doc.Render.Util.SimpleDocTree (

    -- * Type and conversion
    SimpleDocTree(..),
    treeForm,

    -- * Manipulating annotations
    unAnnotateST,
    reAnnotateST,
    alterAnnotationsST,

    -- * Common use case shortcut definitions
    renderSimplyDecorated,
    renderSimplyDecoratedA,
) where



import           Control.Applicative
import           Data.Text           (Text)
import qualified Data.Text           as T
import           Data.Typeable       (Typeable)
import           GHC.Generics

import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Internal
import Data.Text.Prettyprint.Doc.Render.Util.Panic

import qualified Control.Monad.Fail as Fail

#if !(MONOID_IN_PRELUDE)
import Data.Monoid (Monoid (..))
#endif

#if !(FOLDABLE_TRAVERSABLE_IN_PRELUDE)
import Data.Foldable    (Foldable (..))
import Data.Traversable (Traversable (..))
#endif

-- $setup
--
-- (Definitions for the doctests)
--
-- >>> import Data.Text.Prettyprint.Doc hiding ((<>))
-- >>> import qualified Data.Text.IO as T



-- | Simplest possible tree-based renderer.
--
-- For example, here is a document annotated with @()@, and the behaviour is to
-- surround annotated regions with »>>>« and »<<<«:
--
-- >>> let doc = "hello" <+> annotate () "world" <> "!"
-- >>> let stdoc = treeForm (layoutPretty defaultLayoutOptions doc)
-- >>> T.putStrLn (renderSimplyDecorated id (\() x -> ">>>" <> x <> "<<<") stdoc)
-- hello >>>world<<<!
renderSimplyDecorated
    :: Monoid out
    => (Text -> out)       -- ^ Render plain 'Text'
    -> (ann -> out -> out) -- ^ How to modify an element with an annotation
    -> SimpleDocTree ann
    -> out
renderSimplyDecorated text renderAnn = go
  where
    go = \sdt -> case sdt of
        STEmpty        -> mempty
        STChar c       -> text (T.singleton c)
        STText _ t     -> text t
        STLine i       -> text (T.singleton '\n') `mappend` text (textSpaces i)
        STAnn ann rest -> renderAnn ann (go rest)
        STConcat xs    -> foldMap go xs
{-# INLINE renderSimplyDecorated #-}

-- | Version of 'renderSimplyDecoratedA' that allows for 'Applicative' effects.
renderSimplyDecoratedA
    :: (Applicative f, Monoid out)
    => (Text -> f out)         -- ^ Render plain 'Text'
    -> (ann -> f out -> f out) -- ^ How to modify an element with an annotation
    -> SimpleDocTree ann
    -> f out
renderSimplyDecoratedA text renderAnn = go
  where
    go = \sdt -> case sdt of
        STEmpty        -> pure mempty
        STChar c       -> text (T.singleton c)
        STText _ t     -> text t
        STLine i       -> text (T.cons '\n' (textSpaces i))
        STAnn ann rest -> renderAnn ann (go rest)
        STConcat xs    -> fmap mconcat (traverse go xs)
{-# INLINE renderSimplyDecoratedA #-}



-- | A type for parsers of unique results. Token stream »s«, results »a«.
--
-- Hand-written to avoid a dependency on a parser lib.
newtype UniqueParser s a = UniqueParser { runParser :: s -> Maybe (a, s) }
  deriving Typeable

instance Functor (UniqueParser s) where
    fmap f (UniqueParser mx) = UniqueParser (\s ->
        fmap (\(x,s') -> (f x, s')) (mx s))

instance Applicative (UniqueParser s) where
    pure x = UniqueParser (\rest -> Just (x, rest))
    UniqueParser mf <*> UniqueParser mx = UniqueParser (\s -> do
        (f, s') <- mf s
        (x, s'') <- mx s'
        pure (f x, s'') )

instance Monad (UniqueParser s) where
    UniqueParser p >>= f = UniqueParser (\s -> do
        (a', s') <- p s
        let UniqueParser p' = f a'
        p' s' )

#if !(APPLICATIVE_MONAD)
    return = pure
#endif
#if FAIL_IN_MONAD
    fail = Fail.fail
#endif

instance Fail.MonadFail (UniqueParser s) where
    fail _err = empty

instance Alternative (UniqueParser s) where
    empty = UniqueParser (const empty)
    UniqueParser p <|> UniqueParser q = UniqueParser (\s -> p s <|> q s)

data SimpleDocTok ann
    = TokEmpty
    | TokChar Char
    | TokText !Int Text
    | TokLine Int
    | TokAnnPush ann
    | TokAnnPop
    deriving (Eq, Ord, Show, Typeable)

-- | A 'SimpleDocStream' is a linked list of different annotated cons cells
-- ('SText' and then some further 'SimpleDocStream', 'SLine' and then some
-- further 'SimpleDocStream', …). This format is very suitable as a target for a
-- layout engine, but not very useful for rendering to a structured format such
-- as HTML, where we don’t want to do a lookahead until the end of some markup.
-- These formats benefit from a tree-like structure that explicitly marks its
-- contents as annotated. 'SimpleDocTree' is that format.
data SimpleDocTree ann
    = STEmpty
    | STChar Char

    -- | Some layout algorithms use the Since the frequently used 'T.length' of
    -- the 'Text', which scales linearly with its length, we cache it in this
    -- constructor.
    | STText !Int Text

    -- | @Int@ = indentation level for the (next) line
    | STLine !Int

    -- | Annotate the contained document.
    | STAnn ann (SimpleDocTree ann)

    -- | Horizontal concatenation of multiple documents.
    | STConcat [SimpleDocTree ann]
    deriving (Eq, Ord, Show, Generic, Typeable)

-- | Alter the document’s annotations.
--
-- This instance makes 'SimpleDocTree' more flexible (because it can be used in
-- 'Functor'-polymorphic values), but @'fmap'@ is much less readable compared to
-- using @'reAnnotateST'@ in code that only works for @'SimpleDocTree'@ anyway.
-- Consider using the latter when the type does not matter.
instance Functor SimpleDocTree where
    fmap = reAnnotateST

-- | Get the next token, consuming it in the process.
nextToken :: UniqueParser (SimpleDocStream ann) (SimpleDocTok ann)
nextToken = UniqueParser (\sds -> case sds of
    SFail             -> panicUncaughtFail
    SEmpty            -> empty
    SChar c rest      -> Just (TokChar c      , rest)
    SText l t rest    -> Just (TokText l t    , rest)
    SLine i rest      -> Just (TokLine i      , rest)
    SAnnPush ann rest -> Just (TokAnnPush ann , rest)
    SAnnPop rest      -> Just (TokAnnPop      , rest) )

sdocToTreeParser :: UniqueParser (SimpleDocStream ann) (SimpleDocTree ann)
sdocToTreeParser = fmap wrap (many contentPiece)

  where

    wrap :: [SimpleDocTree ann] -> SimpleDocTree ann
    wrap = \sdts -> case sdts of
        []  -> STEmpty
        [x] -> x
        xs  -> STConcat xs

    contentPiece = nextToken >>= \tok -> case tok of
        TokEmpty       -> pure STEmpty
        TokChar c      -> pure (STChar c)
        TokText l t    -> pure (STText l t)
        TokLine i      -> pure (STLine i)
        TokAnnPop      -> empty
        TokAnnPush ann -> do annotatedContents <- sdocToTreeParser
                             TokAnnPop <- nextToken
                             pure (STAnn ann annotatedContents)

-- | Convert a 'SimpleDocStream' to its 'SimpleDocTree' representation.
treeForm :: SimpleDocStream ann -> SimpleDocTree ann
treeForm sdoc = case runParser sdocToTreeParser sdoc of
    Nothing               -> panicSimpleDocTreeConversionFailed
    Just (sdoct, SEmpty)  -> sdoct
    Just (_, _unconsumed) -> panicInputNotFullyConsumed

-- $
--
-- >>> :set -XOverloadedStrings
-- >>> treeForm (layoutPretty defaultLayoutOptions ("lorem" <+> "ipsum" <+> annotate True ("TRUE" <+> annotate False "FALSE") <+> "dolor"))
-- STConcat [STText 5 "lorem",STChar ' ',STText 5 "ipsum",STChar ' ',STAnn True (STConcat [STText 4 "TRUE",STChar ' ',STAnn False (STText 5 "FALSE")]),STChar ' ',STText 5 "dolor"]

-- | Remove all annotations. 'unAnnotate' for 'SimpleDocTree'.
unAnnotateST :: SimpleDocTree ann -> SimpleDocTree xxx
unAnnotateST = alterAnnotationsST (const [])

-- | Change the annotation of a document. 'reAnnotate' for 'SimpleDocTree'.
reAnnotateST :: (ann -> ann') -> SimpleDocTree ann -> SimpleDocTree ann'
reAnnotateST f = alterAnnotationsST (pure . f)

-- | Change the annotation of a document to a different annotation, or none at
-- all. 'alterAnnotations' for 'SimpleDocTree'.
--
-- Note that this is as powerful as 'alterAnnotations', allowing one annotation
-- to become multiple ones, contrary to 'alterAnnotationsS', which cannot do
-- this.
alterAnnotationsST :: (ann -> [ann']) -> SimpleDocTree ann -> SimpleDocTree ann'
alterAnnotationsST re = go
  where
    go = \sdt -> case sdt of
        STEmpty        -> STEmpty
        STChar c       -> STChar c
        STText l t     -> STText l t
        STLine i       -> STLine i
        STConcat xs    -> STConcat (map go xs)
        STAnn ann rest -> Prelude.foldr STAnn (go rest) (re ann)

-- | Collect all annotations from a document.
instance Foldable SimpleDocTree where
    foldMap f = go
      where
        go = \sdt -> case sdt of
            STEmpty        -> mempty
            STChar _       -> mempty
            STText _ _     -> mempty
            STLine _       -> mempty
            STAnn ann rest -> f ann `mappend` go rest
            STConcat xs    -> mconcat (map go xs)

-- | Transform a document based on its annotations, possibly leveraging
-- 'Applicative' effects.
instance Traversable SimpleDocTree where
    traverse f = go
      where
        go = \sdt -> case sdt of
            STEmpty        -> pure STEmpty
            STChar c       -> pure (STChar c)
            STText l t     -> pure (STText l t)
            STLine i       -> pure (STLine i)
            STAnn ann rest -> STAnn <$> f ann <*> go rest
            STConcat xs    -> STConcat <$> traverse go xs
