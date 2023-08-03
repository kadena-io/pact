module Data.Text.Prettyprint.Doc.Render.String (
    renderString,
    renderShowS,
) where

import Data.Text.Prettyprint.Doc.Internal (SimpleDocStream, renderShowS)

-- | Render a 'SimpleDocStream' to a 'String'.
renderString :: SimpleDocStream ann -> String
renderString s = renderShowS s ""
