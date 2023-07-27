-- | Frequently useful definitions for working with general prettyprinters.
module Data.Text.Prettyprint.Doc.Util (
    module Data.Text.Prettyprint.Doc.Util
) where



import           Data.Text                             (Text)
import qualified Data.Text                             as T
import           Data.Text.Prettyprint.Doc.Render.Text
import           Prelude                               hiding (words)
import           System.IO

import Data.Text.Prettyprint.Doc



-- | Split an input into word-sized 'Doc's.
--
-- >>> putDoc (tupled (words "Lorem ipsum dolor"))
-- (Lorem, ipsum, dolor)
words :: Text -> [Doc ann]
words = map pretty . T.words

-- | Insert soft linebreaks between words, so that text is broken into multiple
-- lines when it exceeds the available width.
--
-- >>> putDocW 32 (reflow "Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua.")
-- Lorem ipsum dolor sit amet,
-- consectetur adipisicing elit,
-- sed do eiusmod tempor incididunt
-- ut labore et dolore magna
-- aliqua.
--
-- @
-- 'reflow' = 'fillSep' . 'words'
-- @
reflow :: Text -> Doc ann
reflow = fillSep . words

-- | Render a document with a certain width. Useful for quick-and-dirty testing
-- of layout behaviour. Used heavily in the doctests of this package, for
-- example.
--
-- >>> let doc = reflow "Lorem ipsum dolor sit amet, consectetur adipisicing elit"
-- >>> putDocW 20 doc
-- Lorem ipsum dolor
-- sit amet,
-- consectetur
-- adipisicing elit
-- >>> putDocW 30 doc
-- Lorem ipsum dolor sit amet,
-- consectetur adipisicing elit
putDocW :: Int -> Doc ann -> IO ()
putDocW w doc = renderIO System.IO.stdout (layoutPretty layoutOptions (unAnnotate doc))
  where
    layoutOptions = LayoutOptions { layoutPageWidth = AvailablePerLine w 1 }



-- $setup
--
-- (Definitions for the doctests)
--
-- >>> :set -XOverloadedStrings
