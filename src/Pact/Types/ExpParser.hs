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

-- megaparsec <9.3 backard compatiblity
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

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
  , ParseEnv(..)
  , MkInfo, mkEmptyInfo, mkStringInfo, mkTextInfo
  , ExpParse
  , narrowTry
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
  , keysetNameStr
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
import Control.Monad.Reader
import Control.Arrow (second)
import Prelude hiding (exp)
import Control.Lens hiding (prism)
import Data.Default
import Data.Text (Text,unpack)
import qualified Data.Text as T
import qualified Data.Set as S

import Pact.Types.Exp
import Pact.Types.Pretty hiding (list, sep)
import Pact.Types.PactError (PactError(..),PactErrorType(..))
import Pact.Types.Info
import Pact.Types.KeySet (KeySetName, parseAnyKeysetName)

-- | Exp stream type.
data Cursor = Cursor
  { _cContext :: Maybe (Cursor,Exp Info)
  , _cStream :: [Exp Info]
  } deriving (Show)
instance Default Cursor where def = Cursor def def

-- | Adapt Cursor to MP Stream
instance Stream Cursor where
  type Token Cursor = Exp Info
  type Tokens Cursor = [Exp Info]
  tokenToChunk Proxy = pure
  tokensToChunk Proxy = id
  chunkToTokens Proxy = id
  chunkLength Proxy = length
  chunkEmpty Proxy = null
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

-- | Current env has flag for try-narrow fix.
newtype ParseEnv = ParseEnv
    { _peNarrowTry :: Bool }
instance Default ParseEnv where def = ParseEnv True

type MkInfo = Parsed -> Info

{-# INLINE mkEmptyInfo #-}
mkEmptyInfo :: MkInfo
mkEmptyInfo e = Info (Just (mempty,e))

{-# INLINE mkStringInfo #-}
mkStringInfo :: String -> MkInfo
mkStringInfo = mkTextInfo . T.pack

{-# INLINE mkTextInfo #-}
mkTextInfo :: T.Text -> MkInfo
mkTextInfo s d = Info $ Just (Code code, d)
  where
    code = T.take len $ T.drop offset s
    offset = fromIntegral $ TF.bytes d
    len = _pLength d

type ExpParse s a = ReaderT ParseEnv (StateT (ParseState s) (Parsec Void Cursor)) a

narrowTry :: ExpParse s a -> ExpParse s a -> ExpParse s a
narrowTry legacy narrowed = reader _peNarrowTry >>= \case
  True -> narrowed
  False -> legacy

-- | Run a compile. TODO squint harder at MP errors for better output.
{-# INLINE runCompile #-}
runCompile :: ParseEnv -> ExpParse s a -> ParseState s -> Exp Info -> Either PactError a
runCompile pe act cs a =
  case runParser (runStateT (runReaderT act pe) cs) "" (Cursor Nothing [a]) of
    (Right (r,_)) -> Right r
    (Left (ParseErrorBundle es _)) ->
      Left $ PactError SyntaxError inf [] (prettyString msg)
      where
        msg = intercalate ", " msgs
        -- concat errors in bundle. Info is extracted from
        -- any 'Tokens' found.
        (inf,msgs) = foldr go (def,[]) (toList es)
        go e (i,ms) = case e of
          -- concat errors in TrivialError
          (TrivialError _ itemMay expect) -> foldr go' (i,ms) items
            where expectList = S.toList expect
                  items = maybe expectList (:expectList) itemMay
                  go' item (ri,rmsgs) = case item of
                    Label s -> (ri,toList s:rmsgs)
                    EndOfInput -> (ri,"Unexpected end of input":rmsgs)
                    Tokens (x :| _) -> (getInfo x,rmsgs)
          -- FancyError isn't used but add just in case
          (FancyError _ errs) -> (i,show errs:ms)


{-# INLINE strErr #-}
strErr :: String -> ErrorItem t
strErr = Label . fromList

{-# INLINE tokenErr #-}
tokenErr :: String -> Exp Info -> ExpParse s a
tokenErr s = tokenErr' s . Just

{-# INLINE tokenErr' #-}
tokenErr' :: String -> Maybe (Exp Info) -> ExpParse s a
tokenErr' ty i = failure (Just $ strErr ty) $
  maybe S.empty (\e -> S.singleton (Tokens (e:|[]))) i

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
-- of version 7.0.x, so this might break in future MP versions.
pTokenEpsilon :: forall e s m a. Stream s
  => (Token s -> Maybe a)
  -> S.Set (ErrorItem (Token s))
  -> ParsecT e s m a
pTokenEpsilon test ps = ParsecT $ \s@(State input o pst _) _ _ eok eerr ->
  case take1_ input of
    Nothing ->
      let us = pure EndOfInput
      in eerr (TrivialError o us ps) s
    Just (c,cs) ->
      case test c of
        Nothing ->
          let us = (Just . Tokens . nes) c
          in eerr (TrivialError o us ps)
                  (State input o pst [])
        Just x ->
          eok x (State cs (o + 1) pst []) mempty -- this is only change from 'pToken'
-- {-# INLINE pToken #-}

-- | Call commit continuation with current state.
pCommit :: forall e s m. Ord (Token s) => ParsecT e s m ()
pCommit = ParsecT $ \s cok _ _ _ -> cok () s mempty

-- | Commit any previous recognitions.
commit :: ExpParse s ()
commit = lift (lift pCommit)

-- | Recognize a specific Exp sub-type, non-committing.
{-# INLINE exp #-}
exp :: String -> Prism' (Exp Info) a -> ExpParse s (a,Exp Info)
exp ty prism = do
  t <- current
  let test i = case firstOf prism i of
        Just a -> Just (a,i)
        Nothing -> Nothing
      errs = S.fromList [
        strErr $ "Expected: " ++ ty,
        Tokens (fromList [t])
        ]
  r <- lift $! lift $! pTokenEpsilon test errs
  psCurrent .= snd r
  return r

-- | Recognize any Exp, committing.
{-# INLINE anyExp #-}
anyExp :: ExpParse s (Exp Info)
anyExp = token Just mempty

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

keysetNameStr :: ExpParse s KeySetName
keysetNameStr = parseKsn =<< lit' "keyset-name" _LString
  where
    parseKsn k = case parseAnyKeysetName k of
      Left{} -> syntaxError "invalid keyset name format"
      Right a -> pure a

-- | Recognize a list with specified delimiter, committing.
{-# INLINE list' #-}
list' :: ListDelimiter -> ExpParse s (ListExp Info,Exp Info)
list' d = list >>= \l@(ListExp{..},_) ->
  if _listDelimiter == d then commit >> return l
  else expected $ enlist d (\(s,e)->unpack(s<>"list"<>e))

-- | Recongize a list with specified delimiter and act on contents, committing.
{-# INLINE withList #-}
withList :: ListDelimiter -> (ListExp Info -> ExpParse s a) -> ExpParse s a
withList d act = noTryMay $ list' d >>= enter >>= act >>= \a -> exit >> return a
  where
    noTryMay a = narrowTry (try a) a

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
