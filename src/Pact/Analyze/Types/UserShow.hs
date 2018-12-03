{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
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
  userShowsPrec :: Int -> a -> Text

  userShowList :: [a] -> Text
  userShowList as = "[" <> T.intercalate ", " (userShow <$> as) <> "]"

userShow :: UserShow a => a -> Text
userShow = userShowsPrec 0

parens :: Text -> Text
parens t = "(" <> t <> ")"

parenList :: [Text] -> Text
parenList = parens . T.unwords

instance UserShow Integer where
  userShowsPrec _ = tShow

instance UserShow AlgReal where
  userShowsPrec _ = tShow

instance UserShow (Exp Info) where
  userShowsPrec _ = tShow

instance UserShow Bool where
  userShowsPrec _ = tShow

instance UserShow String where
  userShowsPrec _ = tShow

instance UserShow Int64 where
  userShowsPrec _ = tShow

instance UserShow Text where
  userShowsPrec _ = tShow

instance UserShow a => UserShow (Map Text a) where
  userShowsPrec _ m =
    let go result k a = result <> ", " <> k <> ": " <> userShow a
    in "{ " <> T.drop 2 (Map.foldlWithKey go "" m) <> " }"
