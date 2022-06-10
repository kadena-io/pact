{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}

module Pact.Core.Errors
 ( ErrorSource
 , RenderError(..)
 )where

import Data.Text(Text)
import qualified Data.Text as T

import Pact.Core.Info

type family ErrorSource e where
    ErrorSource () = ()
    ErrorSource LineInfo = Text
    ErrorSource (f a) = ErrorSource a


-- | Class to render errors
-- from the attached source
class RenderError e where
  renders :: e -> Bool
  renderError :: e -> ErrorSource e -> Text

instance RenderError () where
  renders _ = False
  renderError _ _ = mempty

instance RenderError LineInfo where
  renders _ = True
  renderError li source =
    let textSlice = max 1 (_liSpan li)
    in
      "at line " <> T.pack (show (_liLine li)) <> ", column " <> T.pack (show (_liColumn li))
      <> "\n" <> T.take textSlice (T.drop (_liLine li) source)


instance (RenderError e) => RenderError (PactError e) where
  renders (PactError a) = renders a
  renderError (PactError a) source =
    if renders a then "Fatal pact error " <> renderError a source
    else mempty

data PactError a = PactError a deriving Show
