{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
-- |
-- Module      :  Pact.Types.ExpParser
-- Copyright   :  (C) 2018 Stuart Popejoy
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Stuart Popejoy <stuart@kadena.io>
--
-- Parser combinator over 'Exp'.
--

module Pact.Types.ExpParser
  where

import qualified Text.Trifecta.Delta as TF
import Control.Applicative hiding (some,many)
import Text.Megaparsec as MP
import Data.Proxy
import Data.Void
import Data.List.NonEmpty (NonEmpty(..),fromList,toList)
import Data.List
import Control.Monad
import Control.Monad.State
import Control.Arrow (second)
import Prelude hiding (exp)
import Data.String
import Control.Lens hiding (prism)
import Data.Default
import Data.Text (Text,pack,unpack)
import qualified Data.Text as T
import Data.Semigroup ((<>))
import qualified Data.Set as S

import Pact.Types.Exp
import Pact.Types.Runtime (PactError(..),PactErrorType(..))
import Pact.Types.Info


data Cursor = Cursor
  { _cContext :: Maybe (Cursor,Exp Info)
  , _cStream :: [Exp Info]
  } deriving (Show)
instance Default Cursor where def = Cursor def def

-- | adapted from Text.Megaparsec.Stream

defaultAdvance1
  :: Pos               -- ^ Tab width (unused)
  -> SourcePos         -- ^ Current position
  -> t                 -- ^ Current token
  -> SourcePos         -- ^ Incremented position
defaultAdvance1 _width (SourcePos n l c) _t = SourcePos n l (c <> pos1)
{-# INLINE defaultAdvance1 #-}

instance Stream Cursor where
  type Token Cursor = Exp Info
  type Tokens Cursor = [Exp Info]
  tokenToChunk Proxy = pure
  tokensToChunk Proxy = id
  chunkToTokens Proxy = id
  chunkLength Proxy = length
  chunkEmpty Proxy = null
  advance1 Proxy = defaultAdvance1
  advanceN Proxy w = foldl' (defaultAdvance1 w)
  take1_ Cursor{..} = case _cStream of
    [] -> Nothing
    (t:ts) -> Just (t, Cursor _cContext ts)
  takeN_ n s@Cursor{..}
    | n <= 0        = Just ([], s)
    | null _cStream = Nothing
    | otherwise = Just $ second (Cursor _cContext) $ splitAt n _cStream
  takeWhile_ f Cursor{..} = second (Cursor _cContext) $ span f _cStream


data ParseState a = ParseState
  { _psCurrent :: (Exp Info)
  , _psUser :: a
  }
makeLenses ''ParseState


type MkInfo = Parsed -> Info

{-# INLINE mkEmptyInfo #-}
mkEmptyInfo :: MkInfo
mkEmptyInfo e = Info (Just (mempty,e))

{-# INLINE mkStringInfo #-}
mkStringInfo :: String -> MkInfo
mkStringInfo s d = Info (Just (fromString $ take (_pLength d) $
                               drop (fromIntegral $ TF.bytes d) s,d))

{-# INLINE mkTextInfo #-}
mkTextInfo :: T.Text -> MkInfo
mkTextInfo s d = Info (Just (Code $ T.take (_pLength d) $
                             T.drop (fromIntegral $ TF.bytes d) s,d))

type ExpParse s a = StateT (ParseState s) (Parsec Void Cursor) a


{-# INLINE runCompile #-}
runCompile :: ExpParse s a -> ParseState s -> Exp Info -> Either PactError a
runCompile act cs a =
  case runParser (runStateT act cs) "" (Cursor Nothing [a]) of
    (Right (r,_)) -> Right r
    (Left (TrivialError _ (Just err) expect)) -> case err of
      EndOfInput -> case S.toList expect of
        (Tokens (x :| _):_) -> doErr (eInfo x) "unexpected end of input"
        (Label s:_) -> doErr def (toList s)
        er -> doErr def (show er)
      Label ne -> doErr def (toList ne)
      Tokens (x :| _) -> doErr (eInfo x) $ showExpect expect
    (Left e) -> doErr def (show e)
    where doErr i s = Left $ PactError SyntaxError i def (pack s)
          showExpect e = case (labelText $ S.toList e) of
            [] -> show (S.toList e)
            ss -> intercalate "," ss
          labelText [] = []
          labelText (Label s:r) = toList s:labelText r
          labelText (EndOfInput:r) = "End of input":labelText r
          labelText (_:r) = labelText r


{-# INLINE strErr #-}
strErr :: String -> ErrorItem t
strErr = Label . fromList

{-# INLINE tokenErr #-}
tokenErr :: String -> Exp Info -> ExpParse s a
tokenErr s = tokenErr' s . Just

{-# INLINE tokenErr' #-}
tokenErr' :: String -> Maybe (Exp Info) -> ExpParse s a
tokenErr' ty i = failure
  (fmap (\e -> (Tokens (e:|[]))) i)
  (S.singleton (strErr ty))

{-# INLINE context #-}
context :: ExpParse s (Maybe (Exp Info))
context = (fmap snd . _cContext) <$> getInput

{-# INLINE contextInfo #-}
contextInfo :: ExpParse s Info
contextInfo = maybe def eInfo <$> context

{-# INLINE current #-}
current :: ExpParse s (Exp Info)
current = use psCurrent

{-# INLINE syntaxError #-}
syntaxError :: String -> ExpParse s a
syntaxError s = current >>= tokenErr s

{-# INLINE expected #-}
expected :: String -> ExpParse s a
expected s = syntaxError $ "Expected: " ++ s

{-# INLINE unexpected' #-}
unexpected' :: String -> ExpParse s a
unexpected' s = syntaxError $ "Unexpected: " ++ s


{-# INLINE exp #-}
exp :: String -> Prism' (Exp Info) a -> (a-> Either String b) -> ExpParse s (b,Exp Info)
exp ty prism test2 = do
  let test i = case firstOf prism i of
        Just a -> case test2 a of
          Right b -> Right (b,i)
          Left e -> err i e
        Nothing -> err i ("Expected: " ++ ty)
      err i s = Left (pure (Tokens (i:|[])),
                      S.singleton (strErr s))
  r <- context >>= token test
  psCurrent .= snd r
  return r

{-# INLINE exp' #-}
exp' :: String -> Prism' (Exp Info) a -> ExpParse s a
exp' t p = fst <$> exp t p Right

{-# INLINE bareExp #-}
bareExp :: ExpParse s (Exp Info)
bareExp = token Right Nothing

{-# INLINE enter #-}
enter :: (ListExp Info,Exp Info) -> ExpParse s (ListExp Info)
enter (l@ListExp{..},e) = do
  par <- getInput
  setInput $ Cursor (Just (par,e)) _listList
  return l

{-# INLINE exit #-}
exit :: ExpParse s ()
exit = do
  child <- getInput
  case _cContext child of
    Just (p,e) -> setInput p >> (psCurrent .= e)
    Nothing -> failure (Just EndOfInput) def

{-# INLINE atom' #-}
atom' :: (AtomExp Info -> Either String b) -> ExpParse s b
atom' f = fst <$> exp "atom" _EAtom f

{-# INLINE atom #-}
atom :: ExpParse s (AtomExp Info)
atom = exp' "atom" _EAtom

{-# INLINE lit #-}
lit :: ExpParse s (LiteralExp Info)
lit = exp' "literal" _ELiteral

{-# INLINE list #-}
list :: ExpParse s (ListExp Info,Exp Info)
list = exp "list" _EList Right

{-# INLINE sep #-}
sep :: Separator -> ExpParse s ()
sep s = void $ exp "sep" _ESeparator $ \se@SeparatorExp{..} ->
  if _sepSeparator == s then Right se else Left (show s)

{-# INLINE lit' #-}
lit' :: String -> Prism' Literal a -> ExpParse s a
lit' ty prism = lit >>= \LiteralExp{..} -> case firstOf prism _litLiteral of
  Just l -> return l
  Nothing -> expected ty

{-# INLINE str #-}
str :: ExpParse s Text
str = lit' "string" _LString

{-# INLINE list' #-}
list' :: ListDelimiter -> ExpParse s (ListExp Info,Exp Info)
list' d = list >>= \l@(ListExp{..},_) ->
  if _listDelimiter == d then return l
  else expected $ enlist d (\(s,e)->unpack(s<>"list"<>e))

{-# INLINE withList #-}
withList :: ListDelimiter -> (ListExp Info -> ExpParse s a) -> ExpParse s a
withList d act = try $ list' d >>= enter >>= act >>= \a -> exit >> return a

{-# INLINE withList' #-}
withList' :: ListDelimiter -> ExpParse s a -> ExpParse s a
withList' d = withList d . const

{-# INLINE bareAtom #-}
bareAtom :: ExpParse s (AtomExp Info)
bareAtom = atom >>= \a@AtomExp{..} -> case _atomQualifiers of
  (_:_) -> expected "unqualified atom"
  [] -> return a

{-# INLINE symbol #-}
symbol :: Text -> ExpParse s ()
symbol s = void $ atom' $ \AtomExp{..} -> case _atomQualifiers of
  [] | _atomAtom == s -> Right s
  _ -> Left $ "Expected: " ++ unpack s
