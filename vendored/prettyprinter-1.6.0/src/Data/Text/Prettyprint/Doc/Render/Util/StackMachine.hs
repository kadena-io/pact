{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Definitions to write renderers based on looking at a 'SimpleDocStream' as
-- an instruction tape for a stack machine: text is written, annotations are
-- added (pushed) and later removed (popped).
module Data.Text.Prettyprint.Doc.Render.Util.StackMachine (

    -- * Simple, pre-defined stack machines
    --
    -- | These cover most basic use cases where there is not too much special
    -- logic, and all that’s important is how to render text, and how to
    -- add/remove an annotation.
    renderSimplyDecorated,
    renderSimplyDecoratedA,

    -- * General stack machine
    --
    -- | These definitions allow defining a full-blown stack machine renderer,
    -- allowing for arbitrary peeking, popping and what not.
    StackMachine,
    execStackMachine,

    pushStyle,
    unsafePopStyle,
    unsafePeekStyle,
    writeOutput,
) where



import           Control.Applicative
import           Data.Text           (Text)
import qualified Data.Text           as T

import Data.Text.Prettyprint.Doc                   (SimpleDocStream (..))
import Data.Text.Prettyprint.Doc.Render.Util.Panic


-- $setup
--
-- (Definitions for the doctests)
--
-- >>> import Data.Text.Prettyprint.Doc hiding ((<>))
-- >>> import qualified Data.Text.IO as T



-- | Simplest possible stack-based renderer.
--
-- For example, here is a document annotated with @()@, and the behaviour is to
-- write »>>>« at the beginning, and »<<<« at the end of the annotated region:
--
-- >>> let doc = "hello" <+> annotate () "world" <> "!"
-- >>> let sdoc = layoutPretty defaultLayoutOptions doc
-- >>> T.putStrLn (renderSimplyDecorated id (\() -> ">>>") (\() -> "<<<") sdoc)
-- hello >>>world<<<!
--
-- The monoid will be concatenated in a /right associative/ fashion.
renderSimplyDecorated
    :: Monoid out
    => (Text -> out) -- ^ Render plain 'Text'
    -> (ann -> out)  -- ^ How to render an annotation
    -> (ann -> out)  -- ^ How to render the removed annotation
    -> SimpleDocStream ann
    -> out
renderSimplyDecorated text push pop = go []
  where
    go _           SFail               = panicUncaughtFail
    go []          SEmpty              = mempty
    go (_:_)       SEmpty              = panicInputNotFullyConsumed
    go stack       (SChar c rest)      = text (T.singleton c) <> go stack rest
    go stack       (SText _l t rest)   = text t <> go stack rest
    go stack       (SLine i rest)      = text (T.singleton '\n') <> text (T.replicate i " ") <> go stack rest
    go stack       (SAnnPush ann rest) = push ann <> go (ann : stack) rest
    go (ann:stack) (SAnnPop rest)      = pop ann <> go stack rest
    go []          SAnnPop{}           = panicUnpairedPop
{-# INLINE renderSimplyDecorated #-}

-- | Version of 'renderSimplyDecoratedA' that allows for 'Applicative' effects.
renderSimplyDecoratedA
    :: (Applicative f, Monoid out)
    => (Text -> f out) -- ^ Render plain 'Text'
    -> (ann -> f out)  -- ^ How to render an annotation
    -> (ann -> f out)  -- ^ How to render the removed annotation
    -> SimpleDocStream ann
    -> f out
renderSimplyDecoratedA text push pop = go []
  where
    go _           SFail               = panicUncaughtFail
    go []          SEmpty              = pure mempty
    go (_:_)       SEmpty              = panicInputNotFullyConsumed
    go stack       (SChar c rest)      = text (T.singleton c) <++> go stack rest
    go stack       (SText _l t rest)   = text t <++> go stack rest
    go stack       (SLine i rest)      = text (T.singleton '\n') <++> text (T.replicate i " ") <++> go stack rest
    go stack       (SAnnPush ann rest) = push ann <++> go (ann : stack) rest
    go (ann:stack) (SAnnPop rest)      = pop ann <++> go stack rest
    go []          SAnnPop{}           = panicUnpairedPop

    (<++>) = liftA2 mappend
{-# INLINE renderSimplyDecoratedA #-}



-- | @WriterT output StateT [style] a@, but with a strict Writer value.
--
-- The @output@ type is used to append data chunks to, the @style@ is the member
-- of a stack of styles to model nested styles with.
newtype StackMachine output style a = StackMachine ([style] -> (a, output, [style]))
{-# DEPRECATED StackMachine "Writing your own stack machine is probably more efficient and customizable; also consider using »renderSimplyDecorated(A)« instead" #-}

instance Functor (StackMachine output style) where
    fmap f (StackMachine r) = StackMachine (\s ->
        let (x1, w1, s1) = r s
        in (f x1, w1, s1))

instance Monoid output => Applicative (StackMachine output style) where
    pure x = StackMachine (\s -> (x, mempty, s))
    StackMachine f <*> StackMachine x = StackMachine (\s ->
        let (f1, w1, s1) = f s
            (x2, w2, s2) = x s1
            !w12 = w1 <> w2
        in (f1 x2, w12, s2))

instance Monoid output => Monad (StackMachine output style) where
    StackMachine r >>= f = StackMachine (\s ->
        let (x1, w1, s1) = r s
            StackMachine r1 = f x1
            (x2, w2, s2) = r1 s1
            !w12 = w1 <> w2
        in (x2, w12, s2))

-- | Add a new style to the style stack.
pushStyle :: Monoid output => style -> StackMachine output style ()
pushStyle style = StackMachine (\styles -> ((), mempty, style : styles))

-- | Get the topmost style.
--
-- If the stack is empty, this raises an 'error'.
unsafePopStyle :: Monoid output => StackMachine output style style
unsafePopStyle = StackMachine (\stack -> case stack of
    x:xs -> (x, mempty, xs)
    [] -> panicPoppedEmpty )

-- | View the topmost style, but do not modify the stack.
--
-- If the stack is empty, this raises an 'error'.
unsafePeekStyle :: Monoid output => StackMachine output style style
unsafePeekStyle = StackMachine (\styles -> case styles of
    x:_ -> (x, mempty, styles)
    [] -> panicPeekedEmpty )

-- | Append a value to the output end.
writeOutput :: output -> StackMachine output style ()
writeOutput w = StackMachine (\styles -> ((), w, styles))

-- | Run the renderer and retrive the writing end
execStackMachine :: [styles] -> StackMachine output styles a -> (output, [styles])
execStackMachine styles (StackMachine r) = let (_, w, s) = r styles in (w, s)
