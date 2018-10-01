{-# LANGUAGE LambdaCase #-}
-- Hedgehog generators of times
module Analyze.TimeGen where

import           Hedgehog       hiding (Update)
import qualified Hedgehog.Gen   as Gen
import qualified Hedgehog.Range as Range
import qualified Text.Printf    as T

-- with a few problematic ones disabled
data TimeFormatAtom
  = TimePercent
  | Timez
  | TimeN
  | TimeZ
  | Timec
  | TimeR
  | TimeT
  | TimeX
  | Timer
  | TimeP
  | Timep
  | TimeH
  | Timek
  | TimeI
  | Timel
  | TimeM
  | TimeS
  | Timev
  -- | TimeQ
  -- | Times
  | TimeD
  | TimeF
  | Timex
  | TimeY
  | Timey
  | TimeC
  | TimeB
  | Timeb
  | Timeh
  | Timem
  | Timed
  | Timee
  | Timej
  | TimeG
  | Timeg
  -- | Timef
  | TimeV
  | Timeu
  | Timea
  | TimeA
  | TimeU
  | Timew
  | TimeW
  deriving (Bounded, Enum)

showTimeFormatAtom :: TimeFormatAtom -> String
showTimeFormatAtom = \case
  TimePercent -> "%%"
  Timez       -> "%z"
  TimeN       -> "%N"
  TimeZ       -> "%Z"
  Timec       -> "%c"
  TimeR       -> "%R"
  TimeT       -> "%T"
  TimeX       -> "%X"
  Timer       -> "%r"
  TimeP       -> "%P"
  Timep       -> "%p"
  TimeH       -> "%H"
  Timek       -> "%k"
  TimeI       -> "%I"
  Timel       -> "%l"
  TimeM       -> "%M"
  TimeS       -> "%S"
  Timev       -> "%v"
  -- TimeQ       -> "%Q"
  -- Times       -> "%s"
  TimeD       -> "%D"
  TimeF       -> "%F"
  Timex       -> "%x"
  TimeY       -> "%Y"
  Timey       -> "%y"
  TimeC       -> "%C"
  TimeB       -> "%B"
  Timeb       -> "%b"
  Timeh       -> "%h"
  Timem       -> "%m"
  Timed       -> "%d"
  Timee       -> "%e"
  Timej       -> "%j"
  TimeG       -> "%G"
  Timeg       -> "%g"
  -- Timef       -> "%f"
  TimeV       -> "%V"
  Timeu       -> "%u"
  Timea       -> "%a"
  TimeA       -> "%A"
  TimeU       -> "%U"
  Timew       -> "%w"
  TimeW       -> "%W"

showTimeFormat :: TimeFormat -> String
showTimeFormat = mconcat . fmap (either showTimeFormatAtom id)

type TimeFormat = [Either TimeFormatAtom String]

standardTimeFormat :: TimeFormat
standardTimeFormat = [Left TimeY, Right "-", Left Timem, Right "-", Left Timed,
  Right "T", Left TimeH, Right ":", Left TimeM, Right ":", Left TimeS, Left TimeN]

genEither :: MonadGen m => m a -> m b -> m (Either a b)
genEither genA genB = Gen.choice
  [ Left <$> genA
  , Right <$> genB
  ]

genFormat :: MonadGen m => m TimeFormat
genFormat = Gen.list (Range.exponential 0 25) (genEither Gen.enumBounded
  (Gen.string (Range.exponential 0 10) Gen.unicode))

genTimeOfFormat :: MonadGen m => TimeFormat -> m String
genTimeOfFormat fmt = fmap mconcat $ flip traverse fmt $ \case
  Right str -> pure str
  Left atom -> case atom of
    TimePercent -> pure "%"
    Timez       -> T.printf "%+04d" <$> Gen.int (Range.linear (-1200) 1200)
    TimeN       -> T.printf "%+02d:%02d"
      <$> Gen.int (Range.linear (-11) 11)
      <*> Gen.int (Range.linear (-60) 60)
    TimeZ       -> pure "UTC" -- TODO: generalize
    Timec       -> genTimeOfFormat
      [ Left Timea, Right " ", Left Timeb, Left Timee, Right " ",  Left TimeH, Right ":", Left TimeM, Right ":", Left TimeS, Right " ", Left TimeZ, Right " ", Left TimeY]
      -- caution: locale-dependent
    TimeR       -> genTimeOfFormat [Left TimeH, Left TimeM]
    TimeT       -> genTimeOfFormat [Left TimeH, Right ":", Left TimeM, Right ":", Left TimeS]
    TimeX       -> genTimeOfFormat [Left TimeT] -- caution: locale-dependent
    Timer       -> genTimeOfFormat  -- caution: locale-dependent
      [Left TimeI, Right ":", Left TimeM, Right ":", Left TimeS, Right " ", Left Timep]
    TimeP       -> Gen.element ["am", "pm"]
    Timep       -> Gen.element ["AM", "PM"]

    TimeH       -> T.printf "%02d" <$> Gen.int (Range.linear 0 23)
    Timek       -> T.printf "%2d"  <$> Gen.int (Range.linear 0 23)
    TimeI       -> T.printf "%02d" <$> Gen.int (Range.linear 0 12)
    Timel       -> T.printf "%2d"  <$> Gen.int (Range.linear 0 12)
    TimeM       -> T.printf "%02d" <$> Gen.int (Range.linear 0 59)
    TimeS       -> T.printf "%02d" <$> Gen.int (Range.linear 0 60)
    Timev       -> T.printf "%06d" <$> Gen.int (Range.linear 0 999999)

    -- TimeQ       -> "%Q"
    -- Times       -> "%s"
    TimeD       -> genTimeOfFormat [Left Timem, Right "/", Left Timed, Right "/", Left Timey]
    TimeF       -> genTimeOfFormat [Left TimeY, Right "-", Left Timem, Right "-", Left Timed]
    Timex       -> genTimeOfFormat [Left TimeD] -- caution: locale-dependent

    TimeY       -> show <$> Gen.int (Range.linear (-3000) 3000)
    Timey       -> T.printf "%02d" <$> Gen.int (Range.linear 0 99)
    TimeC       -> show <$> Gen.int (Range.linear (-30) 30)

    TimeB       -> Gen.element ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]
    Timeb       -> Gen.element ["Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"]
    Timeh       -> Gen.element ["Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"]
    Timem       -> T.printf "%02d" <$> Gen.int (Range.linear 1 12)
    Timed       -> T.printf "%02d" <$> Gen.int (Range.linear 1 31)
    Timee       -> T.printf "%2d"  <$> Gen.int (Range.linear 1 31)
    Timej       -> T.printf "%03d" <$> Gen.int (Range.linear 1 366)
    TimeG       -> show <$> Gen.int (Range.linear (-3000) 3000)
    Timeg       -> T.printf "%02d" <$> Gen.int (Range.linear 0 99)

--     Timef       ->

    TimeV       -> T.printf "%02d" <$> Gen.int (Range.linear 1 53)
    Timeu       -> show <$> Gen.int (Range.linear 1 7)
    Timea       -> Gen.element ["Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"]
    TimeA       -> Gen.element ["Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"]
    TimeU       -> T.printf "%02d" <$> Gen.int (Range.linear 0 53)
    Timew       -> show <$> Gen.int (Range.linear 0 7)

    TimeW       -> T.printf "%02d" <$> Gen.int (Range.linear 0 53)
