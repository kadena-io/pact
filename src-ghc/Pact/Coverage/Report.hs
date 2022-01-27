{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- |
-- Module      :  Pact.Coverage.Report
-- Copyright   :  (C) 2021 Stuart Popejoy
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Stuart Popejoy <stuart@kadena.io>
--
-- Output lcov tracefile format.

-- Adapted from `hpc-lcov` under BSD-3, see https://hackage.haskell.org/package/hpc-lcov-1.0.1/src/LICENSE
--

module Pact.Coverage.Report
  ( LcovReport(..)
  , FileReport(..)
  , FunctionReport(..)
  , BranchReport(..)
  , LineReport(..)
  , writeReport
  , showReport
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.HashMap.Strict as HM

import Pact.Types.Util

newtype LcovReport = LcovReport (HM.HashMap FilePath FileReport)

instance Semigroup LcovReport where
  LcovReport a <> LcovReport b = LcovReport $ HM.unionWith (<>) a b
instance Monoid LcovReport where
  mempty = LcovReport mempty

data FileReport = FileReport
  { fileReportLocation  :: !FilePath
  , fileReportFunctions :: !(HM.HashMap Text FunctionReport)
  , fileReportBranches  :: !(HM.HashMap Int BranchReport)
  , fileReportLines     :: !(HM.HashMap Int LineReport)
  } deriving (Show, Eq)
instance Semigroup FileReport where
  FileReport a b c d <> FileReport _e f g h = FileReport a
    (HM.unionWith (<>) b f)
    (HM.unionWith (<>) c g)
    (HM.unionWith (<>) d h)

data FunctionReport = FunctionReport
  { functionReportLine :: !Int
  , functionReportName :: !Text
  , functionReportHits :: !Integer
  } deriving (Show, Eq)
instance Semigroup FunctionReport where
  (FunctionReport a b c) <> (FunctionReport _d _e f) = FunctionReport a b (c + f)

data BranchReport = BranchReport
  { branchReportLine      :: !Int
  , branchReportHash      :: !Int
  , branchReportTrueHits  :: !Integer
  , branchReportFalseHits :: !Integer
  } deriving (Show, Eq)
instance Semigroup BranchReport where
  (BranchReport a b c d) <> (BranchReport _e _f g h) = BranchReport a b (c + g) (d + h)

data LineReport = LineReport
  { lineReportLine :: !Int
  , lineReportHits :: !Integer
  } deriving (Show, Eq)
instance Semigroup LineReport where
  (LineReport a b) <> (LineReport _c d) = LineReport a (b + d)

writeReport :: FilePath -> LcovReport -> IO ()
writeReport fp = T.writeFile fp . showReport

showReport :: LcovReport -> Text
showReport (LcovReport fileReports) = T.unlines $ concatMap generateFileReport fileReports
  where
    generateFileReport FileReport{..} = concat
      [ [line "TN" []]
      , [line "SF" [T.pack fileReportLocation]]
      , mmap tShowFunctionDefinition fileReportFunctions
      , mmap tShowFunctionHits fileReportFunctions
      , [line "FNF" [tShow $ length fileReportFunctions]]
      , [line "FNH" [countHits functionReportHits fileReportFunctions]]
      , concatMap generateBranchReport fileReportBranches
      , [line "BRF" [tShow $ length fileReportBranches * 2]] -- multiplying by 2 for true and false branches
      , [line "BRH" [countHits branchReportHits fileReportBranches]]
      , mmap tShowLineReport fileReportLines
      , [line "LF" [tShow $ length fileReportLines]]
      , [line "LH" [countHits lineReportHits fileReportLines]]
      , ["end_of_record"]
      ]

    mmap f = map f . HM.elems

    tShowFunctionDefinition FunctionReport{..} = line "FN" [tShow functionReportLine, functionReportName]

    tShowFunctionHits FunctionReport{..} = line "FNDA" [tShow functionReportHits, functionReportName]

    generateBranchReport BranchReport{..} =
      let mkBranchLine branchNum hits = line "BRDA"
            [ tShow branchReportLine
            , tShow branchReportHash
            , tShow (branchNum :: Int)
            , tShow hits
            ]
      in [mkBranchLine 0 branchReportTrueHits, mkBranchLine 1 branchReportFalseHits]

    tShowLineReport LineReport{..} = line "DA" [tShow lineReportLine, tShow lineReportHits]

    {- Helpers -}

    line :: Text -> [Text] -> Text
    line label info = label <> ":" <> T.intercalate "," info

    countHits :: (a -> Integer) -> HM.HashMap k a -> Text
    countHits f = tShow . length . HM.filter ((> 0) . f)

    branchReportHits BranchReport{..} = branchReportTrueHits + branchReportFalseHits
