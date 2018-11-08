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
  ( Cursor(..)
  , ParseState(..), psCurrent, psUser
  , MkInfo, mkEmptyInfo, mkStringInfo, mkTextInfo
  , ExpParse
  , runCompile
  , tokenErr, tokenErr'
  , syntaxError
  , expected, unexpected'
  , commit
  , exp
  , anyExp
  , enter, exit, context, contextInfo
  , current
  , atom, bareAtom, symbol
  , lit, lit', str
  , list, list', withList, withList'
  , sep
    ) where

import qualified Text.Trifecta.Delta as TF
import Control.Applicative hiding (some,many)
import Text.Megaparsec as MP
import Text.Megaparsec.Internal (ParsecT(..))
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
import qualified Data.Set as S

import Pact.Types.Exp
import Pact.Types.Runtime (PactError(..),PactErrorType(..))
import Pact.Types.Info

-- | Exp stream type.
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

-- | Adapt Cursor to MP Stream, patterned after
-- [Char] instance.
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

-- | Capture last-parsed Exp, plus arbitrary state.
data ParseState a = ParseState
  { _psCurrent :: Exp Info
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

-- | Run a compile. TODO squint harder at MP errors for better output.
{-# INLINE runCompile #-}
runCompile :: ExpParse s a -> ParseState s -> Exp Info -> Either PactError a
runCompile act cs a =
  case runParser (runStateT act cs) "" (Cursor Nothing [a]) of
    (Right (r,_)) -> Right r
    (Left (TrivialError _ (Just err) expect)) -> case err of
      EndOfInput -> case S.toList expect of
        (Tokens (x :| _):_) -> doErr (getInfo x) "unexpected end of input"
        (Label s:_) -> doErr def (toList s)
        er -> doErr def (show er)
      Label ne -> doErr def (toList ne)
      Tokens (x :| _) -> doErr (getInfo x) $ "expected " <> showExpect expect
    (Left e) -> doErr def (show e)
    where doErr i s = Left $ PactError SyntaxError i def (pack s)
          showExpect e = case labelText $ S.toList e of
            [] -> show (S.toList e)
            ss -> intercalate "," ss
          labelText [] = []
          labelText (Label s:r) = toList s:labelText r
          labelText (EndOfInput:r) = "end of expression or input":labelText r
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
  (fmap (\e -> Tokens (e:|[])) i)
  (S.singleton (strErr ty))

{-# INLINE context #-}
context :: ExpParse s (Maybe (Exp Info))
context = fmap snd . _cContext <$> getInput

{-# INLINE contextInfo #-}
contextInfo :: ExpParse s Info
contextInfo = maybe def getInfo <$> context

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

{-# INLINE nes #-}
nes :: a -> NonEmpty a
nes x = x :| []

-- | Test a token in the stream for epsilon/"trivial" acceptance,
-- allowing for further tests on the result before committing.
-- This is copypasta from Megaparsec's implementation of 'token' as
-- of version 6.5.0, so this might break in future MP versions.
pTokenEpsilon :: forall e s m a. Stream s
  => (Token s -> Either ( Maybe (ErrorItem (Token s))
                        , S.Set (ErrorItem (Token s)) ) a)
  -> Maybe (Token s)
  -> ParsecT e s m a
pTokenEpsilon test mtoken = ParsecT $ \s@(State input (pos:|z) tp w) _ _ eok eerr ->
  case take1_ input of
    Nothing ->
      let us = pure EndOfInput
          ps = maybe S.empty (S.singleton . Tokens . nes) mtoken
      in eerr (TrivialError (pos:|z) us ps) s
    Just (c,cs) ->
      case test c of
        Left (us, ps) ->
          let !apos = positionAt1 (Proxy :: Proxy s) pos c
          in eerr (TrivialError (apos:|z) us ps)
                  (State input (apos:|z) tp w)
        Right x ->
          let !npos = advance1 (Proxy :: Proxy s) w pos c
              newstate = State cs (npos:|z) (tp + 1) w
          in eok x newstate mempty -- this is the only change from 'token'

-- | Call commit continuation with current state.
pCommit :: forall e s m. ParsecT e s m ()
pCommit = ParsecT $ \s cok _ _ _ -> cok () s mempty

-- | Commit any previous recognitions.
commit :: ExpParse s ()
commit = lift pCommit

-- | Recognize a specific Exp sub-type, non-committing.
{-# INLINE exp #-}
exp :: String -> Prism' (Exp Info) a -> ExpParse s (a,Exp Info)
exp ty prism = do
  let test i = case firstOf prism i of
        Just a -> Right (a,i)
        Nothing -> err i ("Expected: " ++ ty)
      err i s = Left (pure (Tokens (i:|[])),
                      S.singleton (strErr s))
  r <- context >>= (lift . pTokenEpsilon test)
  psCurrent .= snd r
  return r

-- | Recognize any Exp, committing.
{-# INLINE anyExp #-}
anyExp :: ExpParse s (Exp Info)
anyExp = token Right Nothing

-- | Enter a list context, setting the token stream to its contents
{-# INLINE enter #-}
enter :: (ListExp Info,Exp Info) -> ExpParse s (ListExp Info)
enter (l@ListExp{..},e) = do
  par <- getInput
  setInput $ Cursor (Just (par,e)) _listList
  return l

-- | Exit a list context, resuming a previous parent context.
{-# INLINE exit #-}
exit :: ExpParse s ()
exit = do
  child <- getInput
  case _cContext child of
    Just (p,e) -> setInput p >> (psCurrent .= e)
    Nothing -> failure (Just EndOfInput) def

-- | Recognize an atom, non-committing.
{-# INLINE atom #-}
atom :: ExpParse s (AtomExp Info)
atom = fst <$> exp "atom" _EAtom

-- | Recognized a literal, non-committing.
{-# INLINE lit #-}
lit :: ExpParse s (LiteralExp Info)
lit = fst <$> exp "literal" _ELiteral

-- | Recognize a list, non-committing.
{-# INLINE list #-}
list :: ExpParse s (ListExp Info,Exp Info)
list = exp "list" _EList

-- | Recognize a separator, committing.
{-# INLINE sep #-}
sep :: Separator -> ExpParse s ()
sep s = exp "sep" _ESeparator >>= \(SeparatorExp{..},_) ->
  if _sepSeparator == s then commit else expected (show s)

-- | Recognize a specific literal, non-committing.
{-# INLINE lit' #-}
lit' :: String -> Prism' Literal a -> ExpParse s a
lit' ty prism = lit >>= \LiteralExp{..} -> case firstOf prism _litLiteral of
  Just l -> return l
  Nothing -> expected ty

-- | Recognize a String literal, non-committing.
{-# INLINE str #-}
str :: ExpParse s Text
str = lit' "string" _LString

-- | Recognize a list with specified delimiter, committing.
{-# INLINE list' #-}
list' :: ListDelimiter -> ExpParse s (ListExp Info,Exp Info)
list' d = list >>= \l@(ListExp{..},_) ->
  if _listDelimiter == d then commit >> return l
  else expected $ enlist d (\(s,e)->unpack(s<>"list"<>e))

-- | Recongize a list with specified delimiter and act on contents, committing.
{-# INLINE withList #-}
withList :: ListDelimiter -> (ListExp Info -> ExpParse s a) -> ExpParse s a
withList d act = try $ list' d >>= enter >>= act >>= \a -> exit >> return a

-- | 'withList' without providing ListExp arg to action, committing.
{-# INLINE withList' #-}
withList' :: ListDelimiter -> ExpParse s a -> ExpParse s a
withList' d = withList d . const

-- | Recognize an unqualified "bare" atom, non-committing.
{-# INLINE bareAtom #-}
bareAtom :: ExpParse s (AtomExp Info)
bareAtom = atom >>= \a@AtomExp{..} -> case _atomQualifiers of
  (_:_) -> expected "unqualified atom"
  [] -> return a

-- | Recognize a bare atom with expected text, committing.
{-# INLINE symbol #-}
symbol :: Text -> ExpParse s ()
symbol s = bareAtom >>= \AtomExp{..} ->
  if _atomAtom == s then commit else expected $ unpack s
