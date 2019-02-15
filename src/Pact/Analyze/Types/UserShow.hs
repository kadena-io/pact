{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Type definitions specific to the 'UserShow' typeclass for displaying values
-- to end-users.
module Pact.Analyze.Types.UserShow where

import           Data.Map        (Map)
import qualified Data.Map        as Map
import           Data.SBV        (AlgReal, Int64)
import           Data.Text       (Text)
import qualified Data.Text       as T

import           Pact.Types.Exp  (Exp)
import           Pact.Types.Info (Info)
import           Pact.Types.Util (tShow)

class UserShow a where
  userShowPrec :: Int -> a -> Text

userShow :: UserShow a => a -> Text
userShow = userShowPrec 0

parens :: Text -> Text
parens t = "(" <> t <> ")"

brackets :: Text -> Text
brackets t = "[" <> t <> "]"

parenList :: [Text] -> Text
parenList = parens . T.unwords

instance UserShow Integer where
  userShowPrec _ = tShow

instance UserShow AlgReal where
  userShowPrec _ = tShow

instance UserShow (Exp Info) where
  userShowPrec _ = tShow

instance UserShow Bool where
  userShowPrec _ = tShow

instance UserShow Int64 where
  userShowPrec _ = tShow

instance UserShow Text where
  userShowPrec _ = tShow

instance UserShow a => UserShow (Map Text a) where
  userShowPrec _ m =
    let go result k a = result <> ", " <> k <> ": " <> userShow a
    in "{ " <> T.drop 2 (Map.foldlWithKey go "" m) <> " }"

instance UserShow a => UserShow [a] where
  userShowPrec _ l =
    "[" <> T.intercalate ", " (fmap (userShowPrec 0) l) <> "]"

instance UserShow () where
  userShowPrec _ _ = "()"

instance (UserShow a, UserShow b) => UserShow (a, b) where
  userShowPrec _ (a, b) = "(" <> userShow a <> ", " <> userShow b <> ")"

