{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Render an unannotated 'SimpleDocStream' as plain 'Text'.
module Data.Text.Prettyprint.Doc.Render.Text (
    -- * Conversion to plain 'Text'
    renderLazy, renderStrict,

    -- * Render to a 'Handle'
    renderIO,

    -- ** Convenience functions
    putDoc, hPutDoc
) where



import           Data.Text              (Text)
import qualified Data.Text              as T
import qualified Data.Text.IO           as T
import qualified Data.Text.Lazy         as TL
import qualified Data.Text.Lazy.Builder as TLB
import           System.IO

import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Util.Panic
import Data.Text.Prettyprint.Doc.Render.Util.StackMachine

-- $setup
--
-- (Definitions for the doctests)
--
-- >>> :set -XOverloadedStrings
-- >>> import qualified Data.Text.IO as T
-- >>> import qualified Data.Text.Lazy.IO as TL



-- | @('renderLazy' sdoc)@ takes the output @sdoc@ from a rendering function
-- and transforms it to lazy text.
--
-- >>> let render = TL.putStrLn . renderLazy . layoutPretty defaultLayoutOptions
-- >>> let doc = "lorem" <+> align (vsep ["ipsum dolor", parens "foo bar", "sit amet"])
-- >>> render doc
-- lorem ipsum dolor
--       (foo bar)
--       sit amet
renderLazy :: SimpleDocStream ann -> TL.Text
renderLazy = TLB.toLazyText . renderSimplyDecorated TLB.fromText (pure mempty) (pure mempty)

-- | @('renderStrict' sdoc)@ takes the output @sdoc@ from a rendering function
-- and transforms it to strict text.
renderStrict :: SimpleDocStream ann -> Text
renderStrict = TL.toStrict . renderLazy



-- | @('renderIO' h sdoc)@ writes @sdoc@ to the file @h@.
--
-- >>> renderIO System.IO.stdout (layoutPretty defaultLayoutOptions "hello\nworld")
-- hello
-- world
--
-- This function is more efficient than @'T.hPutStr' h ('renderStrict' sdoc)@,
-- since it writes to the handle directly, skipping the intermediate 'Text'
-- representation.
renderIO :: Handle -> SimpleDocStream ann -> IO ()
renderIO h = go
  where
    go :: SimpleDocStream ann -> IO ()
    go = \sds -> case sds of
        SFail              -> panicUncaughtFail
        SEmpty             -> pure ()
        SChar c rest       -> do hPutChar h c
                                 go rest
        SText _ t rest     -> do T.hPutStr h t
                                 go rest
        SLine n rest       -> do hPutChar h '\n'
                                 T.hPutStr h (T.replicate n " ")
                                 go rest
        SAnnPush _ann rest -> go rest
        SAnnPop rest       -> go rest

-- | @('putDoc' doc)@ prettyprints document @doc@ to standard output. Uses the
-- 'defaultLayoutOptions'.
--
-- >>> putDoc ("hello" <+> "world")
-- hello world
--
-- @
-- 'putDoc' = 'hPutDoc' 'stdout'
-- @
putDoc :: Doc ann -> IO ()
putDoc = hPutDoc stdout

-- | Like 'putDoc', but instead of using 'stdout', print to a user-provided
-- handle, e.g. a file or a socket. Uses the 'defaultLayoutOptions'.
--
-- @
-- main = 'withFile' filename (\h -> 'hPutDoc' h doc)
--   where
--     doc = 'vcat' ["vertical", "text"]
--     filename = "someFile.txt"
-- @
--
-- @
-- 'hPutDoc' h doc = 'renderIO' h ('layoutPretty' 'defaultLayoutOptions' doc)
-- @
hPutDoc :: Handle -> Doc ann -> IO ()
hPutDoc h doc = renderIO h (layoutPretty defaultLayoutOptions doc)
