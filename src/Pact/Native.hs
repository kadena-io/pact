{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE MultiWayIf #-}
-- |
-- Module      :  Pact.Native
-- Copyright   :  (C) 2016 Stuart Popejoy
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Stuart Popejoy <stuart@kadena.io>
--
-- Pact builtins/standard library.
--

module Pact.Native
    ( natives
    , nativeDefs
    , moduleToMap
    , distinctDef
    , enforceDef
    , enforceOneDef
    , enumerateDef
    , pactVersionDef
    , formatDef
    , strToIntDef
    , strToListDef
    , concatDef
    , intToStrDef
    , hashDef
    , ifDef
    , readDecimalDef
    , readIntegerDef
    , readStringDef
    , baseStrToInt
    , mapDef
    , zipDef
    , foldDef
    , makeListDef
    , reverseDef
    , filterDef
    , sortDef
    , whereDef
    , composeDef
    , lengthDef
    , takeDef
    , dropDef
    , atDef
    , chainDataSchema
    , cdChainId, cdBlockHeight, cdBlockTime, cdSender, cdGasLimit, cdGasPrice
    , describeNamespaceSchema
    , dnUserGuard, dnAdminGuard, dnNamespaceName
    , cdPrevBlockHash
    , encodeTokenMessage
    ) where

import Control.Arrow hiding (app, first)
import Control.Exception.Safe
import Control.Lens hiding (parts,Fold,contains)
import Control.Monad
import Control.Monad.IO.Class
import qualified Data.Attoparsec.Text as AP
import Data.Bifunctor (first)
import Data.Binary (get, put)
import Data.Binary.Get (Get, runGetOrFail, getByteString, isEmpty)
import Data.Binary.Put (Put, runPut, putByteString)
import Data.Bool (bool)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Base64.URL as B64URL
import qualified Data.Char as Char
import Data.Bits
import Data.Decimal (Decimal)
import Data.Default
import Data.Functor(($>))
import Data.Foldable
import Data.List (isPrefixOf)
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as M
import qualified Data.List as L (nubBy)
import Data.Ratio ((%))
import qualified Data.Set as S
import Data.Text (Text, pack, unpack)
import qualified Data.Text as T
import qualified Data.Text as Text
import qualified Data.Text.Encoding as T
import Data.WideWord.Word256
import Pact.Time
import qualified Data.Vector as V
import qualified Data.Vector.Algorithms.Intro as V
import Numeric
import Text.Read (readMaybe)

import Pact.Eval
import Pact.Native.Capabilities
import Pact.Native.Db
import Pact.Native.Decrypt
import Pact.Native.Guards
import Pact.Native.Internal
import Pact.Native.Keysets
import Pact.Native.Ops
import Pact.Native.SPV
import Pact.Native.Time
import Pact.Native.Pairing(zkDefs)
import Pact.Parse
import Pact.Runtime.Utils(lookupFreeVar)
import Pact.Types.Hash
import Pact.Types.Names
import Pact.Types.PactValue
import Pact.Types.Pretty hiding (list)
import Pact.Types.Purity
import Pact.Types.Runtime
import Pact.Types.Version
import Pact.Types.Namespace
import Crypto.Hash.PoseidonNative (poseidon)
import Crypto.Hash.HyperlaneMessageId (hyperlaneMessageId)

import qualified Pact.JSON.Encode as J
import qualified Pact.JSON.Decode as J

-- | All production native modules.
natives :: [NativeModule]
natives =
  [ langDefs
  , dbDefs
  , timeDefs
  , opDefs
  , keyDefs
  , capDefs
  , spvDefs
  , decryptDefs
  , guardDefs
  , zkDefs
  , poseidonHackAChainDefs
  , hyperlaneDefs
  ]


-- | Production native modules as a dispatch map.
nativeDefs :: HM.HashMap Text Ref
nativeDefs = mconcat $ map moduleToMap natives

moduleToMap :: NativeModule -> HM.HashMap Text Ref
moduleToMap = HM.fromList . map (asString *** Direct) . snd


lengthDef :: NativeDef
lengthDef = defRNative "length" length' (funType tTyInteger [("x",listA)])
 ["(length [1 2 3])", "(length \"abcdefgh\")", "(length { \"a\": 1, \"b\": 2 })"]
 "Compute length of X, which can be a list, a string, or an object."

listA :: Type n
listA = mkTyVar "a" [TyList (mkTyVar "l" []),TyPrim TyString,TySchema TyObject (mkSchemaVar "o") def]

enforceDef :: NativeDef
enforceDef = defNative "enforce" enforce
  (funType tTyBool [("test",tTyBool),("msg",tTyString)])
  [ExecErrExample "(enforce (!= (+ 2 2) 4) \"Chaos reigns\")"]
  "Fail transaction with MSG if pure expression TEST is false. Otherwise, returns true."
  where

    enforce :: NativeFun e
    enforce i as = runSysOnly $ gasUnreduced i as $
      ifExecutionFlagSet FlagDisablePact45
      (mapM reduce as >>= enforce' i)
      (enforceLazy i as)

    enforce' :: RNativeFun e
    enforce' i [TLiteral (LBool b') _,TLitString msg]
        | b' = return $ TLiteral (LBool True) def
        | otherwise = failTx (_faInfo i) $ pretty msg
    enforce' i as = argsError i as

    enforceLazy i [cond, msg] =
      reduce cond >>= \case
        TLiteral (LBool b') _ ->
          if b' then
            return (TLiteral (LBool True) def)
          else reduce msg >>= \case
            TLitString msg' -> failTx (_faInfo i) $ pretty msg'
            e -> isOffChainForkedError FlagDisablePact47 >>= \case
              OffChainError -> evalError' i $ "Invalid message argument, expected string " <> pretty e
              OnChainError -> evalError' i $ "Invalid message argument, expected string, received argument of type: " <> pretty (typeof' e)
        cond' -> reduce msg >>= argsError i . reverse . (:[cond'])
    enforceLazy i as = mapM reduce as >>= argsError i

enforceOneDef :: NativeDef
enforceOneDef =
  defNative "enforce-one" enforceOne (funType tTyBool [("msg",tTyString),("tests",TyList tTyBool)])
  ["(enforce-one \"Should succeed on second test\" [(enforce false \"Skip me\") (enforce (= (+ 2 2) 4) \"Chaos reigns\")])"]
  "Run TESTS in order (in pure context, plus keyset enforces). If all fail, fail transaction. Short-circuits on first success."
  where
    enforceBool i e = reduce e >>= \case
      tl@(TLiteral (LBool cond) _) -> if cond then pure (Just tl) else pure Nothing
      _ -> evalError' i "Failure: expected boolean result in enforce-one"
    catchTxFailure :: PactError -> Eval e (Maybe (Term Name))
    catchTxFailure e = case peType e of
      TxFailure -> pure Nothing
      _ -> throwM e
    enforceOne :: NativeFun e
    enforceOne i as@[msg,TList conds _ _] = runReadOnly i $
      gasUnreduced i as $ do
        msg' <- reduce msg >>= \case
          TLitString s -> return s
          _ -> argsError' i as
        let tryCond r@Just {} _ = return r
            tryCond Nothing cond = isExecutionFlagSet FlagDisablePact44 >>= \case
              True -> do
                g <- getGas
                catch (Just <$> reduce cond)
                  -- TODO: instead of catching all here, make pure violations
                  -- independently catchable
                  (\(_ :: SomeException) -> putGas g *> return Nothing)
              False -> catch (enforceBool i cond) catchTxFailure
        r <- foldM tryCond Nothing conds
        case r of
          Nothing -> failTx (_faInfo i) $ pretty msg'
          Just b' -> return b'
    enforceOne i as = argsError' i as

tryDef :: NativeDef
tryDef =
  defNative "try" try' (funType a [("default", a), ("action", a)])
  ["(try 3 (enforce (= 1 2) \"this will definitely fail\"))"
  ,LitExample "(expect \"impure expression fails and returns default\" \"default\" \
   \(try \"default\" (with-read accounts id {'ccy := ccy}) ccy))"
  ]
  "Attempt a pure ACTION, returning DEFAULT in the case of failure. Pure expressions \
  \are expressions which do not do i/o or work with non-deterministic state in contrast \
  \to impure expressions such as reading and writing to a table."
  where
    try' :: NativeFun e
    try' i as@[da, action] = gasUnreduced i as $ isExecutionFlagSet FlagDisablePact44 >>= \case
      True -> do
        ra <- reduce da
        g1 <- getGas
        -- TODO: instead of catching all here, make pure violations
        -- independently catchable
        catch (runReadOnly i $ reduce action) $ \(_ :: SomeException) -> do
          putGas g1
          return ra
      False -> do
        catch (runReadOnly i $ reduce action) $ \(e :: PactError) -> case peType e of
          TxFailure -> reduce da
          _ -> throwM e
    try' i as = argsError' i as

pactVersionDef :: NativeDef
pactVersionDef = setTopLevelOnly $ defRNative "pact-version"
  pactVersion'
  (funType tTyString [])
  ["(pact-version)"]
  "Obtain current pact build version."
  where
  -- note the 4.2.1 hardcode is
  -- for compat across versions since it was previously set
  -- by the cabal macro.
  -- After pact 4.3.1, this is a local-only call and thus
  -- we can use the cabal-generated version.
  pactVersion' :: RNativeFun e
  pactVersion' i _ = do
    cond <- isExecutionFlagSet FlagDisablePact431
    if cond then pure (toTerm compatVersion)
    else checkNonLocalAllowed i *> pure (toTerm pactVersion)
    where
    compatVersion :: Text
    compatVersion = "4.2.1"


formatDef :: NativeDef
formatDef =
  defRNative "format" format
  (funType tTyString [("template",tTyString),("vars",TyList TyAny)])
  ["(format \"My {} has {}\" [\"dog\" \"fleas\"])"]
  "Interpolate VARS into TEMPLATE using {}."

strToListDef :: NativeDef
strToListDef = defGasRNative "str-to-list" strToList
  (funType (TyList tTyString) [("str", tTyString)] )
  [ "(str-to-list \"hello\")"
  , "(concat (map (+ \" \") (str-to-list \"abcde\")))"
  ]
  "Takes STR and returns a list of single character strings"

concatDef :: NativeDef
concatDef = defGasRNative "concat" concat'
  (funType tTyString [("str-list", TyList tTyString)] )
  [ "(concat [\"k\" \"d\" \"a\"])"
  , "(concat (map (+ \" \") (str-to-list \"abcde\")))"
  ]
  "Takes STR-LIST and concats each of the strings in the list, returning the resulting string"

strToIntDef :: NativeDef
strToIntDef = defRNative "str-to-int" strToInt
  (funType tTyInteger [("str-val", tTyString)] <>
   funType tTyInteger [("base", tTyInteger), ("str-val", tTyString)])
  ["(str-to-int 16 \"abcdef123456\")"
  ,"(str-to-int \"123456\")"
  ,"(str-to-int 64 \"q80\")"
  ]
  "Compute the integer value of STR-VAL in base 10, or in BASE if specified. \
  \STR-VAL can be up to 512 chars in length. \
  \BASE must be between 2 and 16, or 64 to perform unpadded base64url conversion. \
  \Each digit must be in the correct range for the base."

intToStrDef :: NativeDef
intToStrDef = defRNative "int-to-str" intToStr
  (funType tTyString [("base",tTyInteger),("val",tTyInteger)])
  ["(int-to-str 16 65535)","(int-to-str 64 43981)"]
  "Represent integer VAL as a string in BASE. BASE can be 2-16, or 64 for unpadded base64URL. \
  \Only positive values are allowed for base64URL conversion."
  where
    intToStr _ [b'@(TLitInteger base),v'@(TLitInteger v)]
      | base >= 2 && base <= 16 =
          return $ toTerm $ T.pack $
          showIntAtBase base Char.intToDigit v ""
      | base == 64 && v >= 0 = doBase64 v
      | base == 64 = evalError' v' "Only positive values allowed for base64URL conversion."
      | otherwise = evalError' b' "Invalid base, must be 2-16 or 64"
    intToStr i as = argsError i as
    doBase64 v = return $ toTerm $ toB64UrlUnpaddedText $ integerToBS v

hashDef :: NativeDef
hashDef = defRNative "hash" hash' (funType tTyString [("value",a)])
  ["(hash \"hello\")", "(hash { 'foo: 1 })"]
  "Compute BLAKE2b 256-bit hash of VALUE represented in unpadded base64-url. \
  \Strings are converted directly while other values are \
  \converted using their JSON representation. Non-value-level arguments are not allowed."
  where
    hash' :: RNativeFun e
    hash' i as = case as of
      [TLitString s] -> go $ T.encodeUtf8 s
      [a'] -> enforcePactValue a' >>= go . J.encodeStrict
      _ -> argsError i as
      where go = return . tStr . asString . pactHash

ifDef :: NativeDef
ifDef = defNative "if" if' (funType a [("cond",tTyBool),("then",a),("else",a)])
  ["(if (= (+ 2 2) 4) \"Sanity prevails\" \"Chaos reigns\")"]
  "Test COND. If true, evaluate THEN. Otherwise, evaluate ELSE."
  where

    if' :: NativeFun e
    if' i as@[cond,then',else'] = gasUnreduced i as $ reduce cond >>= \case
      TLiteral (LBool c') _ -> reduce (if c' then then' else else')
      t -> isOffChainForkedError FlagDisablePact47 >>= \case
        OffChainError -> evalError' i $ "if: conditional not boolean: " <> pretty t
        OnChainError -> evalError' i $ "if: conditional not boolean, received value of type: " <> pretty (typeof' t)

    if' i as = argsError' i as


readDecimalDef :: NativeDef
readDecimalDef = defRNative "read-decimal" readDecimal
  (funType tTyDecimal [("key",tTyString)])
  [LitExample "(defun exec ()\n   (transfer (read-msg \"from\") (read-msg \"to\") (read-decimal \"amount\")))"]
  "Parse KEY string or number value from top level of message data body as decimal."

  where
    readDecimal :: RNativeFun e
    readDecimal i [TLitString key] = do
      (ParsedDecimal a') <- parseMsgKey i "read-decimal" key
      return $ toTerm a'
    readDecimal i as = argsError i as

readIntegerDef :: NativeDef
readIntegerDef = defRNative "read-integer" readInteger
  (funType tTyInteger [("key",tTyString)])
  [LitExample "(read-integer \"age\")"]
  "Parse KEY string or number value from top level of message data body as integer."

  where
    readInteger :: RNativeFun e
    readInteger i [TLitString key] = do
      (ParsedInteger a') <- parseMsgKey i "read-integer" key
      return $ toTerm a'
    readInteger i as = argsError i as

readStringDef :: NativeDef
readStringDef = defRNative "read-string" readString
  (funType tTyString [("key",tTyString)])
  [LitExample "(read-string \"sender\")"]
  "Parse KEY string or number value from top level of message data body as string."

  where
    readString :: RNativeFun e
    readString i [TLitString key] = do
      txt <- parseMsgKey i "read-string" key
      return $ tStr txt
    readString i as = argsError i as


toGuardPactValue :: Guard (Term Name) -> Either Text (Guard PactValue)
toGuardPactValue g = case g of
  (GUser (UserGuard n ts)) -> do
    pvs <- map elideModRefInfo <$> traverse toPactValue ts
    return (GUser (UserGuard n pvs))
  (GCapability (CapabilityGuard n ts pid)) -> do
    pvs <- map elideModRefInfo <$> traverse toPactValue ts
    return (GCapability (CapabilityGuard n pvs pid))
  (GKeySet k) -> Right (GKeySet k)
  (GKeySetRef k) -> Right (GKeySetRef k)
  (GModule m) -> Right (GModule m)
  (GPact p) -> Right (GPact p)


fromGuardPactValue :: Guard PactValue -> Guard (Term Name)
fromGuardPactValue g = case g of
  (GUser (UserGuard n ts)) -> GUser (UserGuard n (map fromPactValue ts))
  (GCapability (CapabilityGuard n ts pid)) ->
    GCapability (CapabilityGuard n (map fromPactValue ts) pid)
  (GKeySet k) -> GKeySet k
  (GKeySetRef k) -> GKeySetRef k
  (GModule m) -> GModule m
  (GPact p) -> GPact p

toNamespacePactValue :: Info -> Namespace (Term Name) -> Eval e (Namespace PactValue)
toNamespacePactValue info (Namespace name userg adming) = do
  usergPv <- throwEitherText EvalError info
             "Failed converting namespace user guard to pact value"
             (toGuardPactValue userg)
  admingPv <- throwEitherText EvalError info
              "Failed converting namespace admin guard to pact value"
              (toGuardPactValue adming)
  return (Namespace name usergPv admingPv)

fromNamespacePactValue :: Namespace PactValue -> Namespace (Term Name)
fromNamespacePactValue (Namespace n userg adming) =
  Namespace n (fromGuardPactValue userg) (fromGuardPactValue adming)

dnUserGuard :: FieldKey
dnUserGuard = "user-guard"

dnAdminGuard :: FieldKey
dnAdminGuard = "admin-guard"

dnNamespaceName :: FieldKey
dnNamespaceName = "namespace-name"

describeNamespaceSchema :: NativeDef
describeNamespaceSchema = defSchema "described-namespace"
  "Schema type for data returned from 'describe-namespace'."
  [ (dnUserGuard, tTyGuard Nothing)
  , (dnAdminGuard, tTyGuard Nothing)
  , (dnNamespaceName, tTyString)
  ]

describeNamespaceDef :: NativeDef
describeNamespaceDef = setTopLevelOnly $ defGasRNative
  "describe-namespace" describeNamespace
  (funType (tTyObject dnTy) [("ns", tTyString)])
  [LitExample "(describe-namespace 'my-namespace)"]
  "Describe the namespace NS, returning a row object containing \
  \the user and admin guards of the namespace, as well as its name."
  where
    dnTy = TyUser (snd describeNamespaceSchema)

    describeNamespace :: GasRNativeFun e
    describeNamespace i as = case as of
      [TLitString nsn] -> do
        readRow (getInfo i) Namespaces (NamespaceName nsn) >>= \case
          Just ns@(Namespace nsn' user admin) -> do
            let guardTermOf g = TGuard (fromPactValue <$> g) def

            computeGas' i (GPostRead (ReadNamespace ns)) $
              pure $ toTObject dnTy def
                [ (dnUserGuard, guardTermOf user)
                , (dnAdminGuard, guardTermOf admin)
                , (dnNamespaceName, toTerm $ renderCompactText nsn')
                ]
          Nothing -> evalError' i $ "Namespace not defined: " <> pretty nsn
      _ -> argsError i as

defineNamespaceDef :: NativeDef
defineNamespaceDef = setTopLevelOnly $ defGasRNative "define-namespace" defineNamespace
  (funType tTyString [("namespace", tTyString), ("user-guard", tTyGuard Nothing), ("admin-guard", tTyGuard Nothing)])
  [LitExample "(define-namespace 'my-namespace (read-keyset 'user-ks) (read-keyset 'admin-ks))"]
  "Create a namespace called NAMESPACE where ownership and use of the namespace is controlled by GUARD. \
  \If NAMESPACE is already defined, then the guard previously defined in NAMESPACE will be enforced, \
  \and GUARD will be rotated in its place."
  where
    defineNamespace :: GasRNativeFun e
    defineNamespace i as = case as of
      [TLitString nsn, TGuard userg _, TGuard adming _] -> case parseName (_faInfo i) nsn of
        Left _ -> evalError' i $ "invalid namespace name format: " <> pretty nsn
        Right _ -> go i nsn userg adming
      _ -> argsError i as

    go fi nsn ug ag = do
      let name = NamespaceName nsn
          info = _faInfo fi
          newNs = Namespace name ug ag
      newNsPactValue <- toNamespacePactValue info newNs
      mOldNs <- readRow info Namespaces name
      szVer <- getSizeOfVersion
      case (fromNamespacePactValue <$> mOldNs) of
        Just ns@(Namespace _ _ oldg) -> do
          -- if namespace is defined, enforce old guard
          nsPactValue <- toNamespacePactValue info ns
          computeGas (Right fi) (GPostRead (ReadNamespace nsPactValue))
          enforceGuard fi oldg
          computeGas' fi (GPreWrite (WriteNamespace newNsPactValue) szVer) $
            writeNamespace info name newNsPactValue
        Nothing -> do
          enforcePolicy info name newNs
          computeGas' fi (GPreWrite (WriteNamespace newNsPactValue) szVer) $
            writeNamespace info name newNsPactValue

    enforcePolicy info nn ns = do
      policy <- view eeNamespacePolicy
      allowNs <- case policy of
        SimpleNamespacePolicy f -> return $ f (Just ns)
        SmartNamespacePolicy _ fun -> applyNsPolicyFun fun fun nn ns
      unless allowNs $ evalError info "Namespace definition not permitted"

    writeNamespace info nn ns =
      success ("Namespace defined: " <> asString nn) $ do
        writeRow info Write Namespaces nn ns


    applyNsPolicyFun :: HasInfo i => i -> QualifiedName -> NamespaceName
                     -> (Namespace (Term Name)) -> Eval e Bool
    applyNsPolicyFun fi fun nn ns = do
      let i = getInfo fi
      refm <- resolveRef i (QName fun)
      def' <- case refm of
        (Just (Ref d@TDef {})) -> return d
        Just t -> evalError i $ "invalid ns policy fun: " <> pretty t
        Nothing -> evalError i $ "ns policy fun not found: " <> pretty fun
      asBool =<< apply (App def' [] i) mkArgs
      where
        asBool (TLiteral (LBool allow) _) = return allow
        asBool t = isOffChainForkedError FlagDisablePact47 >>= \case
          OffChainError -> evalError' fi $ "Unexpected return value from namespace policy: " <> pretty t
          OnChainError -> evalError' fi $ "Unexpected return value from namespace policy, received value of type: " <> pretty (typeof' t)

        mkArgs = [toTerm (asString nn),TGuard (_nsAdmin ns) def]


namespaceDef :: NativeDef
namespaceDef = setTopLevelOnly $ defGasRNative "namespace" namespace
  (funType tTyString [("namespace", tTyString)])
  [LitExample "(namespace 'my-namespace)"]
  "Set the current namespace to NAMESPACE. All expressions that occur in a current \
  \transaction will be contained in NAMESPACE, and once committed, may be accessed \
  \via their fully qualified name, which will include the namespace. Subsequent \
  \namespace calls in the same tx will set a new namespace for all declarations \
  \until either the next namespace declaration, or the end of the tx."
  where
    namespace :: GasRNativeFun e
    namespace i as = case as of
      [TLitString nsn] ->
        ifExecutionFlagSet FlagDisablePact47
          (go i nsn)
          (if T.null nsn then
             goEmpty i
           else go i nsn)
      _ -> argsError i as

    goEmpty fa = computeGas' fa (GUnreduced [])
      $ success "Namespace reset to root"
      $ evalRefs . rsNamespace .= Nothing

    go fa ns = do
      let name = NamespaceName ns
          info = _faInfo fa

      mNs <- readRow info Namespaces name
      case fromNamespacePactValue <$> mNs of
        Just n@(Namespace ns' g _) -> do
          nsPactValue <- toNamespacePactValue info n
          computeGas' fa (GPostRead (ReadNamespace nsPactValue)) $ do
            -- Old behavior enforces ns at declaration.
            -- New behavior enforces at ns-related action:
            -- 1. Module install (NOT at module upgrade)
            -- 2. Interface install (Interfaces non-upgradeable)
            -- 3. Namespaced keyset definition (once #351 is in)
            whenExecutionFlagSet FlagDisablePact44 $ enforceGuard fa g
            success ("Namespace set to " <> (asString ns')) $
              evalRefs . rsNamespace .= (Just n)
        Nothing  -> evalError info $
          "namespace: '" <> pretty name <> "' not defined"

cdChainId :: FieldKey
cdChainId = "chain-id"
cdBlockHeight :: FieldKey
cdBlockHeight = "block-height"
cdBlockTime :: FieldKey
cdBlockTime = "block-time"
cdPrevBlockHash :: FieldKey
cdPrevBlockHash = "prev-block-hash"
cdSender :: FieldKey
cdSender = "sender"
cdGasLimit :: FieldKey
cdGasLimit = "gas-limit"
cdGasPrice :: FieldKey
cdGasPrice = "gas-price"

chainDataSchema :: NativeDef
chainDataSchema = defSchema "public-chain-data"
  "Schema type for data returned from 'chain-data'."
    [ (cdChainId, tTyString)
    , (cdBlockHeight, tTyInteger)
    , (cdBlockTime, tTyTime)
    , (cdPrevBlockHash, tTyString)
    , (cdSender, tTyString)
    , (cdGasLimit, tTyInteger)
    , (cdGasPrice, tTyDecimal)
    ]

chainDataDef :: NativeDef
chainDataDef = defRNative "chain-data" chainData
    (funType (tTyObject pcTy) [])
    ["(chain-data)"]
    "Get transaction public metadata. Returns an object with 'chain-id', 'block-height', \
    \'block-time', 'prev-block-hash', 'sender', 'gas-limit', 'gas-price', and 'gas-fee' fields."
  where
    pcTy = TyUser (snd chainDataSchema)
    chainData :: RNativeFun e
    chainData _ [] = do
      PublicData{..} <- view eePublicData

      let PublicMeta{..} = _pdPublicMeta
          toTime = toTerm . fromPosixTimestampMicros

      pure $ toTObject TyAny def
        [ (cdChainId, toTerm _pmChainId)
        , (cdBlockHeight, toTerm _pdBlockHeight)
        , (cdBlockTime, toTime _pdBlockTime)
        , (cdPrevBlockHash, toTerm _pdPrevBlockHash)
        , (cdSender, toTerm _pmSender)
        , (cdGasLimit, toTerm _pmGasLimit)
        , (cdGasPrice, toTerm _pmGasPrice)
        ]
    chainData i as = argsError i as

mapDef :: NativeDef
mapDef = defNative "map" map'
  (funType (TyList a) [("app",lam b a),("list",TyList b)])
  ["(map (+ 1) [1 2 3])"]
  "Apply APP to each element in LIST, returning a new list of results."

foldDef :: NativeDef
foldDef = defNative "fold" fold'
  (funType a [("app",lam2 a b a),("init",a),("list",TyList b)])
  ["(fold (+) 0 [100 10 5])"]
  "Iteratively reduce LIST by applying APP to last result and element, starting with INIT."

makeListDef :: NativeDef
makeListDef = defGasRNative "make-list" makeList (funType (TyList a) [("length",tTyInteger),("value",a)])
  ["(make-list 5 true)"]
  "Create list by repeating VALUE LENGTH times."

enumerateDef :: NativeDef
enumerateDef = defGasRNative "enumerate" enumerate
  (funType (TyList tTyInteger) [("from", tTyInteger), ("to", tTyInteger),("inc", tTyInteger)]
  <> funType (TyList tTyInteger) [("from", tTyInteger), ("to", tTyInteger)])
  ["(enumerate 0 10 2)"
   , "(enumerate 0 10)"
   , "(enumerate 10 0)"]
  $ T.intercalate " "
  [ "Returns a sequence of numbers from FROM to TO (both inclusive) as a list."
  , "INC is the increment between numbers in the sequence."
  , "If INC is not given, it is assumed to be 1."
  , "Additionally, if INC is not given and FROM is greater than TO assume a value for INC of -1."
  , "If FROM equals TO, return the singleton list containing FROM, irrespective of INC's value."
  , "If INC is equal to zero, this function will return the singleton list containing FROM."
  , "If INC is such that FROM + INC > TO (when FROM < TO) or FROM + INC < TO (when FROM > TO) return the singleton list containing FROM."
  , "Lastly, if INC is such that FROM + INC < TO (when FROM < TO) or FROM + INC > TO (when FROM > TO), then this function fails."
  ]

reverseDef :: NativeDef
reverseDef = defRNative "reverse" reverse' (funType (TyList a) [("list",TyList a)])
  ["(reverse [1 2 3])"] "Reverse LIST."

filterDef :: NativeDef
filterDef = defNative "filter" filter'
  (funType (TyList a) [("app",lam a tTyBool),("list",TyList a)])
  ["(filter (compose (length) (< 2)) [\"my\" \"dog\" \"has\" \"fleas\"])"]
  "Filter LIST by applying APP to each element. For each true result, the original value is kept."

distinctDef :: NativeDef
distinctDef = defGasRNative "distinct" distinct
  (funType (TyList a) [("values", TyList a)])
  ["(distinct [3 3 1 1 2 2])"]
  $ T.intercalate " "
  [ "Returns from a homogeneous list of VALUES a list with duplicates removed."
  , "The original order of the values is preserved."]

sortDef :: NativeDef
sortDef = defGasRNative "sort" sort'
  (funType (TyList a) [("values",TyList a)] <>
   funType (TyList (tTyObject (mkSchemaVar "o"))) [("fields",TyList tTyString),("values",TyList (tTyObject (mkSchemaVar "o")))])
  ["(sort [3 1 2])", "(sort ['age] [{'name: \"Lin\",'age: 30} {'name: \"Val\",'age: 25}])"]
  "Sort a homogeneous list of primitive VALUES, or objects using supplied FIELDS list."

whereDef :: NativeDef
whereDef = defNative (specialForm Where) where'
  (funType tTyBool [("field",tTyString),("app",lam a tTyBool),("value",tTyObject (mkSchemaVar "row"))])
  ["(filter (where 'age (> 20)) [{'name: \"Mary\",'age: 30} {'name: \"Juan\",'age: 15}])"]
  "Utility for use in 'filter' and 'select' applying APP to FIELD in VALUE."

composeDef :: NativeDef
composeDef = defNative "compose" compose (funType c [("x",lam a b),("y", lam b c),("value",a)])
  ["(filter (compose (length) (< 2)) [\"my\" \"dog\" \"has\" \"fleas\"])"]
  "Compose X and Y, such that X operates on VALUE, and Y on the results of X."

takeDef :: NativeDef
takeDef = defRNative "take" take' takeDrop
  [ "(take 2 \"abcd\")"
  , "(take (- 3) [1 2 3 4 5])"
  , "(take ['name] { 'name: \"Vlad\", 'active: false})"
  ]
  "Take COUNT values from LIST (or string), or entries having keys in KEYS from OBJECT. If COUNT is negative, take from end. If COUNT exceeds the interval (-2^63,2^63), it is truncated to that range."

dropDef :: NativeDef
dropDef = defRNative "drop" drop' takeDrop
  [ "(drop 2 \"vwxyz\")"
  , "(drop (- 2) [1 2 3 4 5])"
  , "(drop ['name] { 'name: \"Vlad\", 'active: false})"
  ]
  "Drop COUNT values from LIST (or string), or entries having keys in KEYS from OBJECT. If COUNT is negative, drop from end. If COUNT exceeds the interval (-2^63,2^63), it is truncated to that range."

zipDef :: NativeDef
zipDef = defNative "zip" zip' zipTy
  [ "(zip (+) [1 2 3 4] [4 5 6 7])"
  , "(zip (-) [1 2 3 4] [4 5 6])"
  , "(zip (+) [1 2 3] [4 5 6 7])"
  ]
  "Combine two lists with some function f, into a new list, the length of which is the length of the shortest list."


atDef :: NativeDef
atDef = defRNative "at" at'
  (  funType a [("idx",tTyInteger),("list",TyList (mkTyVar "l" []))]
  <> funType a [("idx",tTyString),("object",tTyObject (mkSchemaVar "o"))]
  )
  ["(at 1 [1 2 3])", "(at \"bar\" { \"foo\": 1, \"bar\": 2 })"]
  "Index LIST at IDX, or get value with key IDX from OBJECT."

asciiConst :: NativeDef
asciiConst = defConst "CHARSET_ASCII"
  "Constant denoting the ASCII charset"
  tTyInteger
  (toTerm (0 :: Integer))

latin1Const :: NativeDef
latin1Const = defConst "CHARSET_LATIN1"
  "Constant denoting the Latin-1 charset ISO-8859-1"
  tTyInteger
  (toTerm (1 :: Integer))

-- | Check that a given string conforms to a specified character set.
-- Supported character sets include latin (ISO 8859-1)
--
isCharsetDef :: NativeDef
isCharsetDef =
  defRNative "is-charset" isCharset
  (funType tTyBool [("charset", tTyInteger), ("input", tTyString)])
  [ "(is-charset CHARSET_ASCII \"hello world\")"
  , "(is-charset CHARSET_ASCII \"I am nÖt ascii\")"
  , "(is-charset CHARSET_LATIN1 \"I am nÖt ascii, but I am latin1!\")"
  ]
  "Check that a string INPUT conforms to the a supported character set CHARSET.       \
  \Character sets currently supported are: 'CHARSET_LATIN1' (ISO-8859-1), and         \
  \'CHARSET_ASCII' (ASCII). Support for sets up through ISO 8859-5 supplement will be \
  \added in the future."
  where
    isCharset :: RNativeFun e
    isCharset i as = case as of
      [TLitInteger cs, TLitString t] -> case cs of
        0 -> go Char.isAscii t
        1 -> go Char.isLatin1 t
        _ -> evalError' i $ "Unsupported character set: " <> pretty cs
      _ -> argsError i as

    go k = return . toTerm . T.all k

langDefs :: NativeModule
langDefs =
    ("General",[
     ifDef
    ,mapDef
    ,foldDef

    ,defRNative "list" list
     (funType (TyList TyAny) [("elems",TyAny)])
     ["(list 1 2 3)"]
     "Create list from ELEMS. Deprecated in Pact 2.1.1 with literal list support."

    ,makeListDef
    ,enumerateDef
    ,reverseDef
    ,filterDef
    ,sortDef
    ,whereDef
    ,composeDef
    ,lengthDef
    ,takeDef
    ,dropDef
    ,zipDef
    ,defRNative "remove" remove (funType (tTyObject (mkSchemaVar "o")) [("key",tTyString),("object",tTyObject (mkSchemaVar "o"))])
     ["(remove \"bar\" { \"foo\": 1, \"bar\": 2 })"]
     "Remove entry for KEY from OBJECT."
    ,atDef
    ,enforceDef
    ,enforceOneDef
    ,tryDef
    ,formatDef
    ,defRNative "pact-id" pactId (funType tTyString []) []
     "Return ID if called during current pact execution, failing if not."
    ,readDecimalDef
    ,readIntegerDef
    ,readStringDef
    ,defRNative "read-msg" readMsg (funType a [] <> funType a [("key",tTyString)])
     [LitExample "(defun exec ()\n   (transfer (read-msg \"from\") (read-msg \"to\") (read-decimal \"amount\")))"]
     "Read KEY from top level of message data body, or data body itself if not provided. \
     \Coerces value to their corresponding pact type: String -> string, Number -> integer, Boolean -> bool, \
     \List -> list, Object -> object."
    ,defRNative "tx-hash" txHash (funType tTyString []) ["(tx-hash)"]
     "Obtain hash of current transaction as a string."
    ,defNative (specialForm Bind) bind
     (funType a [("src",tTyObject row),("binding",TySchema TyBinding row def)])
     ["(bind { \"a\": 1, \"b\": 2 } { \"a\" := a-value } a-value)"]
     "Special form evaluates SRC to an object which is bound to with BINDINGS over subsequent body statements."
    ,defRNative "typeof" typeof'' (funType tTyString [("x",a)])
     ["(typeof \"hello\")"] "Returns type of X as string."
    ,distinctDef
    ,setTopLevelOnly $ defRNative "list-modules" listModules
     (funType (TyList tTyString) []) [] "List modules available for loading."
    ,defGasRNative (specialForm YieldSF) yield
     (funType yieldv [("object",yieldv)] <>
      funType yieldv [("object", yieldv), ("target-chain",tTyString)])
     [ LitExample "(yield { \"amount\": 100.0 })"
     , LitExample "(yield { \"amount\": 100.0 } \"some-chain-id\")"
     ]
     "Yield OBJECT for use with 'resume' in following pact step. With optional argument TARGET-CHAIN, \
     \target subsequent step to execute on targeted chain using automated SPV endorsement-based dispatch."
    ,defNative (specialForm Resume) resume
     (funType a [("binding",TySchema TyBinding (mkSchemaVar "r") def)]) []
     "Special form binds to a yielded object value from the prior step execution in a pact. \
     \If yield step was executed on a foreign chain, enforce endorsement via SPV."
    ,pactVersionDef
    ,setTopLevelOnly $ defRNative "enforce-pact-version" enforceVersion
     (funType tTyBool [("min-version",tTyString)] <>
      funType tTyBool [("min-version",tTyString),("max-version",tTyString)])
    ["(enforce-pact-version \"2.3\")"]
    "Enforce runtime pact version as greater than or equal MIN-VERSION, and less than or equal MAX-VERSION. \
    \Version values are matched numerically from the left, such that '2', '2.2', and '2.2.3' would all allow '2.2.3'."
    ,defRNative "contains" contains
    (funType tTyBool [("value",a),("list",TyList a)] <>
     funType tTyBool [("key",a),("object",tTyObject (mkSchemaVar "o"))] <>
     funType tTyBool [("value",tTyString),("string",tTyString)])
    [ "(contains 2 [1 2 3])"
    , "(contains 'name { 'name: \"Ted\", 'age: 72 })"
    , "(contains \"foo\" \"foobar\")"
    ]
    "Test that LIST or STRING contains VALUE, or that OBJECT has KEY entry."
    ,defNative "constantly" constantly
     (funType a [("value",a),("ignore1",b)] <>
      funType a [("value",a),("ignore1",b),("ignore2",c)] <>
      funType a [("value",a),("ignore1",b),("ignore2",c),("ignore3",d)])
     ["(filter (constantly true) [1 2 3])"]
     "Lazily ignore arguments IGNORE* and return VALUE."
    ,defRNative "identity" identity (funType a [("value",a)])
     ["(map (identity) [1 2 3])"] "Return provided value."
    ,defNative "continue"  continueNested
     (funType TyAny [("value", TyAny)]) [LitExample "(continue f)"]
     "Continue a previously started nested defpact."
    ,strToIntDef
    ,strToListDef
    ,concatDef
    ,intToStrDef
    ,hashDef
    ,defineNamespaceDef
    ,describeNamespaceDef
    ,namespaceDef
    ,chainDataDef
    ,chainDataSchema
    ,isCharsetDef
    ,asciiConst
    ,latin1Const
    ,base64Encode
    ,base64decode
    ])
    where
          d = mkTyVar "d" []
          row = mkSchemaVar "row"
          yieldv = TySchema TyObject (mkSchemaVar "y") def

obj :: Type n
obj = tTyObject (mkSchemaVar "o")

listStringA :: Type n
listStringA = mkTyVar "a" [TyList (mkTyVar "l" []),TyPrim TyString]

takeDrop :: FunTypes n
takeDrop = funType listStringA [("count",tTyInteger),("list",listStringA)] <>
           funType obj [("keys",TyList tTyString),("object",obj)]

zipTy :: FunTypes n
zipTy = funType (TyList c) [("f", TyFun $ funType' c [("x", a), ("y", b)]),("list1", TyList a), ("list2", TyList b)]

lam :: Type v -> Type v -> Type v
lam x y = TyFun $ funType' y [("x",x)]

lam2 :: Type v -> Type v -> Type v -> Type v
lam2 x y z = TyFun $ funType' z [("x",x),("y",y)]

a, b, c :: Type n
a = mkTyVar "a" []
b = mkTyVar "b" []
c = mkTyVar "c" []

map' :: NativeFun e
map' i as@[tLamToApp -> TApp app _,l] = gasUnreduced i as $ reduce l >>= \case
           TList ls _ _ -> (\b' -> TList b' TyAny def) <$> forM ls (apply app . pure)
           t ->
            isOffChainForkedError FlagDisablePact47 >>= \case
              OffChainError -> evalError' i $ "map: expecting list: " <> pretty (abbrev t)
              OnChainError -> evalError' i $ "map: expecting list, received argument of type: " <> pretty (typeof' t)
map' i as = argsError' i as

list :: RNativeFun e
list i as = return $ TList (V.fromList as) TyAny (_faInfo i) -- TODO, could set type here

makeList :: GasRNativeFun e
makeList i [TLitInteger len,value] = case typeof value of
  Right ty -> do
    ts <- ifExecutionFlagSet' FlagDisablePact48 Pact43IntThreshold Pact48IntThreshold
    ga <- ifExecutionFlagSet' FlagDisablePact45 (GMakeList len) (GMakeList2 len Nothing ts)
    computeGas' i ga $ return $
      toTListV ty def $ V.replicate (fromIntegral len) value
  Left ty -> evalError' i $ "make-list: invalid value type: " <> pretty ty
makeList i as = argsError i as

enumerate :: GasRNativeFun e
enumerate i = \case
    [TLitInteger from', TLitInteger to', TLitInteger inc] ->
      createEnumerateList from' to' inc
    [TLitInteger from', TLitInteger to'] ->
      createEnumerateList from' to' $ bool 1 (-1) (from' > to')
    as -> argsError i as
  where

    computeList
      :: Integer
      -- ^ the length of the list
      -> Integer
      -- ^ size of the largest element.
      -> V.Vector Integer
      -- ^ The generated vector
      -> Eval e (Term Name)
    computeList len sz v = do
      ts <- ifExecutionFlagSet' FlagDisablePact48 Pact43IntThreshold Pact48IntThreshold
      ga <- ifExecutionFlagSet' FlagDisablePact45 (GMakeList len) (GMakeList2 len (Just sz) ts)
      computeGas' i ga $ pure $ toTListV tTyInteger def $ fmap toTerm v

    step to' inc acc
      | acc > to', inc > 0 = Nothing
      | acc < to', inc < 0 = Nothing
      | otherwise = Just (acc, acc + inc)

    createEnumerateList from' to' inc
      | from' == to' = computeList 1 from' (V.singleton from')
      | inc == 0 = computeList 1 0 mempty
      | from' < to', from' + inc < from' =
        evalError' i "enumerate: increment diverges below from interval bounds."
      | from' > to', from' + inc > from' =
        evalError' i "enumerate: increment diverges above from interval bounds."
      | otherwise = do
        let g' = succ $ (abs (from' - to')) `div` (abs inc)
        computeList g' (max (abs from') (abs to')) $ V.unfoldr (step to' inc) from'

reverse' :: RNativeFun e
reverse' i [l@TList{}] = do
  unlessExecutionFlagSet FlagDisablePact48 $ computeGas (Right i) (GReverse listLen)
  pure $ over tList V.reverse l
  where
    listLen = V.length $ view tList l
reverse' i args = argsError i args

format :: RNativeFun e
format i [TLitString s,TList es _ _] = do
  let parts = T.splitOn "{}" s
      plen = length parts
  if | plen == 1 -> return $ tStr s
     | plen - length es > 1 -> evalError' i "format: not enough arguments for template"
     | otherwise -> do
        args <- ifExecutionFlagSet FlagDisablePact48 formatLegacyArgs formatFullGassedArgs
        return $ tStr $ T.concat $ alternate parts (take (plen - 1) args)
  where
    repVal (PLiteral (LString t)) = t
    repVal t = renderCompactText t

    repTerm (TLitString t) = t
    repTerm t = renderCompactText t

    alternate (x:xs) ys = x : alternate ys xs
    alternate _ _ = []

    formatLegacyArgs = do
      let args = repTerm <$> V.toList es
          !totalArgLen = sum (T.length <$> args)
          inputLength = T.length s
      unlessExecutionFlagSet FlagDisablePact44 $ computeGas (Right i) (GConcatenation inputLength totalArgLen)
      pure args
    formatFullGassedArgs = do
      vals <- traverse enforcePactValue es
      void $ computeGas (Right i) (GFormatValues s vals)
      pure $ repVal <$> V.toList vals
format i as = argsError i as

fold' :: NativeFun e
fold' i as@[tLamToApp -> app@TApp {},initv,l] = gasUnreduced i as $ reduce l >>= \case
           TList ls _ _ -> reduce initv >>= \initv' ->
                         foldM (\r a' -> apply (_tApp app) [r,a']) initv' ls
           t ->
            isOffChainForkedError FlagDisablePact47 >>= \case
              OffChainError -> evalError' i $ "fold: expecting list: " <> pretty (abbrev t)
              OnChainError -> evalError' i $ "fold: expecting list, received argument of type: " <> pretty (typeof' t)
fold' i as = argsError' i as


filter' :: NativeFun e
filter' i as@[tLamToApp -> app@TApp {},l] = gasUnreduced i as $ reduce l >>= \case
  TList ls lt _ -> fmap (toTListV lt def) $ (`V.filterM` ls) $ \a' -> do
    t <- apply (_tApp app) [a']
    case t of
      (TLiteral (LBool bo) _) -> return bo
      _ -> ifExecutionFlagSet FlagDisablePact42
             (return False)
             (evalError' i $ "filter: expected closure to return bool: " <> pretty app)
  t -> isOffChainForkedError FlagDisablePact47 >>= \case
      OffChainError -> evalError' i $ "filter: expecting list: " <> pretty (abbrev t)
      OnChainError -> evalError' i $ "filter: expecting list, received argument of type: " <> pretty (typeof' t)
filter' i as = argsError' i as


length' :: RNativeFun e
length' _ [TList ls _ _] = return $ toTerm (length ls)
length' _ [TLitString s] = return $ toTerm (T.length s)
length' _ [TObject (Object ps _ _ _) _] = return $ toTerm (length ps)
length' i as = argsError i as

take' :: RNativeFun e
take' _ [TLitInteger c',TList l t _] = return $ TList (tordV V.take c' l) t def
take' _ [TLitInteger c',TLitString l] = return $ toTerm $ pack $ tordL take c' (unpack l)
take' _ [TList {..},TObject (Object (ObjectMap o) oTy _ _) _] = asKeyList _tList >>= \l ->
  return $ toTObjectMap oTy def $ ObjectMap $ M.restrictKeys o l
take' i as = argsError i as

drop' :: RNativeFun e
drop' _ [TLitInteger c',TList l t _] = return $ TList (tordV V.drop c' l) t def
drop' _ [TLitInteger c',TLitString l] = return $ toTerm $ pack $ tordL drop c' (unpack l)
drop' _ [TList {..},TObject (Object (ObjectMap o) oTy _ _) _] = asKeyList _tList >>= \l ->
  return $ toTObjectMap oTy def $ ObjectMap $ M.withoutKeys o l
drop' i as = argsError i as

zip' :: NativeFun e
zip' i as@[tLamToApp -> TApp app _, l1, l2] = gasUnreduced i as $ (,) <$> reduce l1 <*> reduce l2 >>= \case
 -- reduce list terms first, though unfortunately
  -- this means that lambdas within lists won't work for now.
  (TList l1' _ _, TList l2' _ _) -> do
    terms <- sequence $ V.zipWith (\e1 e2 -> apply app [e1, e2]) l1' l2'
    pure $ TList terms TyAny (getInfo i)
  (l, r) -> argsError i [l, r]
zip' i as = argsError' i as

asKeyList :: V.Vector (Term Name) -> Eval e (S.Set FieldKey)
asKeyList l = fmap (S.fromList . V.toList) . V.forM l $ \t -> case t of
  TLitString k -> return $ FieldKey k
  _ -> evalError (_tInfo t) "String required"

-- | "take or drop" handling negative "take/drop from reverse"
tord :: (l -> l) -> (Int -> l -> l) -> Integer -> l -> l
tord rev f c' l | c' >= 0 = f (fromIntegral (limit c')) l
                | otherwise = rev $ f (fromIntegral (negate (limit c'))) (rev l)
  where
    -- {Vector,List}.{take,drop} ops produce undefined behavior (crashing for
    -- Vector) when provided an int outside of the range (-2^63, 2^63). This
    -- function truncates an integer to this range.
    limit :: Integer -> Integer
    limit i
      | i < -bound = -bound
      | i > bound = bound
      | otherwise = i
      where
        bound = (2 ^ (63 :: Integer)) - 1

tordV
  :: (Int -> V.Vector a -> V.Vector a)
     -> Integer -> V.Vector a -> V.Vector a
tordV = tord V.reverse

tordL :: (Int -> [a] -> [a]) -> Integer -> [a] -> [a]
tordL = tord reverse

at' :: RNativeFun e
at' _ [li@(TLitInteger idx),TList ls _ _] =
    case ls V.!? fromIntegral idx of
      Just t -> return t
      Nothing -> evalError (_tInfo li) $ "at: bad index " <>
        pretty idx <> ", length " <> pretty (length ls)
at' _ [idx,TObject (Object ls _ _ _) _] = lookupObj idx ls
at' i as = argsError i as

lookupObj :: Term n -> ObjectMap (Term n) -> Eval m (Term n)
lookupObj t@(TLitString idx) (ObjectMap ls) = case M.lookup (FieldKey idx) ls of
  Just v -> return v
  Nothing -> evalError (_tInfo t) $ "key not found in object: " <> pretty idx
lookupObj t _ = evalError (_tInfo t) $ "object lookup only supported with strings"

remove :: RNativeFun e
remove _ [TLitString key,TObject (Object (ObjectMap ps) t _ _) _] =
  return $ toTObjectMap t def $ ObjectMap $ M.delete (FieldKey key) ps
remove i as = argsError i as

compose :: NativeFun e
compose i as@[tLamToApp -> appA@TApp {},tLamToApp -> appB@TApp {},v] = gasUnreduced i as $ do
  v' <- reduce v
  a' <- apply (_tApp appA) [v']
  apply (_tApp appB) [a']
compose i as = argsError' i as

readMsg :: RNativeFun e
readMsg i [TLitString key] = fromPactValue <$> parseMsgKey i "read-msg" key
readMsg i [] = fromPactValue <$> parseMsgKey' i "read-msg" Nothing
readMsg i as = argsError i as

pactId :: RNativeFun e
pactId i [] = toTerm <$> getPactId i
pactId i as = argsError i as

bind :: NativeFun e
bind i as@[src,TBinding ps bd (BindSchema _) bi] = gasUnreduced i as $
  reduce src >>= bindObjectLookup >>= bindReduce ps bd bi
bind i as = argsError' i as

bindObjectLookup :: Term Name -> Eval e (Text -> Maybe (Term Name))
bindObjectLookup (TObject (Object (ObjectMap o) _ _ _) _) =
  return $ \s -> M.lookup (FieldKey s) o
bindObjectLookup t = isOffChainForkedError FlagDisablePact47 >>= \case
  OffChainError -> evalError (_tInfo t) $ "bind: expected object: " <> pretty t
  OnChainError -> evalError (_tInfo t) $ "bind: expected object, received value of type: " <> pretty (typeof' t)

typeof'' :: RNativeFun e
typeof'' _ [t] = return $ tStr $ typeof' t
typeof'' i as = argsError i as

listModules :: RNativeFun e
listModules i _ = do
  unlessExecutionFlagSet FlagDisablePact411 $ checkNonLocalAllowed i
  mods <- keys (_faInfo i) Modules
  return $ toTermList tTyString $ map asString mods

yield :: GasRNativeFun e
yield i as = case as of
  [u@(TObject t _)] -> go t Nothing u
  [u@(TObject t _), (TLitString cid)] -> go t (Just $ ChainId cid) u
  _ -> argsError i as
  where
    go (Object o _ _ _) tid u = do
      eym <- use evalPactExec
      case eym of
        Nothing -> evalError' i "Yield not in defpact context"
        Just pe -> do
          o' <- fmap elideModRefInfo <$> traverse enforcePactValue o
          y <- case tid of
            Nothing -> return $ Yield o' Nothing Nothing
            Just t -> do
              sourceChain <- ifExecutionFlagSet FlagDisablePact40 (return Nothing) $
                fmap Just $ view $ eePublicData . pdPublicMeta . pmChainId
              if _peStepHasRollback pe
                then evalError' i "Cross-chain yield not allowed in step with rollback"
                else fmap (\p -> Yield o' p sourceChain) $ provenanceOf i t
          szVer <- getSizeOfVersion
          computeGas' i (GPreWrite (WriteYield y) szVer) $ do
            evalPactExec . _Just . peYield .= Just y
            return u

resume :: NativeFun e
resume i as = case as of
  [TBinding ps bd (BindSchema _) bi] -> do
    computeGas (Right i) (GUnreduced as)
    rm <- preview $ eePactStep . _Just . psResume . _Just
    case rm of
      Nothing -> evalError' i "Resume: no yielded value in context"
      Just y -> do
        computeGas' i (GPostRead (ReadYield y)) $ do
          o <- fmap fromPactValue . _yData <$> enforceYield i y
          l <- bindObjectLookup (toTObjectMap TyAny def o)
          bindReduce ps bd bi l
  _ -> argsError' i as

where' :: NativeFun e
where' i as@[k',tLamToApp -> app@TApp{},r'] = gasUnreduced i as $ ((,) <$> reduce k' <*> reduce r') >>= \case
  (k,r@TObject {}) -> lookupObj k (_oObject $ _tObject r) >>= \v -> apply (_tApp app) [v]
  _ -> argsError' i as
where' i as = argsError' i as

distinct :: GasRNativeFun e
distinct i = \case
    [l@(TList v _ _ )] | V.null v -> pure l
    [TList{..}] -> _tList
        & V.toList
        & L.nubBy termEq
        & V.fromList
        & toTListV _tListType def
        & pure
        & computeGas' i (GDistinct $ V.length _tList)
    as -> argsError i as


sort' :: GasRNativeFun e
sort' _ [l@(TList v _ _)] | V.null v = pure l
sort' i [TList{..}] = computeGas' i (GSort (V.length _tList)) $ liftIO $ do
  m <- V.thaw _tList
  (`V.sortBy` m) $ \x y -> case (x,y) of
    (TLiteral xl _,TLiteral yl _) -> xl `compare` yl
    _ -> EQ
  toTListV _tListType def <$> V.freeze m
sort' fa [TList fields _ fi,l@(TList vs lty _)]
  | V.null fields = evalError fi "Empty fields list"
  | V.null vs = return l
  | otherwise = do
      computeGas (Right fa) (GSort (V.length vs))
      fields' <- asKeyList fields
      computeGas' fa (GSortFieldLookup (S.size fields')) $ liftIO $ do
        m <- V.thaw vs
        (`V.sortBy` m) $ \x y -> case (x,y) of
          (TObject (Object (ObjectMap xo) _ _ _) _,TObject (Object (ObjectMap yo) _ _ _) _) ->
            let go field EQ = case (M.lookup field xo, M.lookup field yo) of
                  (Just (TLiteral lx _), Just (TLiteral ly _)) -> lx `compare` ly
                  _ -> EQ
                go _ ne = ne
            in foldr go EQ fields'
          _ -> EQ
        toTListV lty def <$> V.freeze m
sort' i as = argsError i as


-- See: note in pact-version native
-- about the value
enforceVersion :: RNativeFun e
enforceVersion i as = do
  cond <- isExecutionFlagSet FlagDisablePact431
  pactVersion'
    <- if cond then pure compatVersion else checkNonLocalAllowed i $> pactVersion
  case as of
    [TLitString minVersion] -> doMin minVersion pactVersion' $> toTerm True
    [TLitString minVersion,TLitString maxVersion] ->
      doMin minVersion pactVersion' >> doMax maxVersion pactVersion' $> toTerm True
    _ -> argsError i as
  where
    compatVersion :: Text
    compatVersion = "4.2.1"
    doMin = doMatch "minimum" (>) (<)
    doMax = doMatch "maximum" (<) (>)
    doMatch msg failCmp succCmp fullV pactVersion' = do
      foldM_ matchPart False $ zip (T.splitOn "." pactVersion') (T.splitOn "." fullV)
      where
        parseNum :: Text -> Text -> Eval e Integer
        parseNum orgV s = case AP.parseOnly AP.decimal s of
          Left _ -> evalError' i $ "Invalid version component: " <> pretty (orgV,s)
          Right v -> return v

        matchPart True _ = return True
        matchPart _ (pv,mv)  = do
          pv' <- parseNum pactVersion' pv
          mv' <- parseNum fullV mv
          when (mv' `failCmp` pv') $ evalError' i $
            "Invalid pact version " <> pretty pactVersion' <>
            ", " <> msg <> " allowed: " <> pretty fullV
          return (mv' `succCmp` pv')

contains :: RNativeFun e
contains _i [val,TList {..}] = return $ toTerm $ searchTermList val _tList
contains _i [TLitString k,TObject (Object (ObjectMap o) _ _ _) _] =
  return $ toTerm $ M.member (FieldKey k) o
contains _i [TLitString s,TLitString t] = return $ toTerm $ T.isInfixOf s t
contains i as = argsError i as

searchTermList :: (Foldable t, Eq n) => Term n -> t (Term n) -> Bool
searchTermList val = foldl search False
  where search True _ = True
        search _ t = t `termEq` val


constantly :: NativeFun e
constantly i (v:_) = gasUnreduced i [v] $ reduce v
constantly i as = argsError' i as

identity :: RNativeFun e
identity _ [a'] = return a'
identity i as = argsError i as

concat' :: GasRNativeFun e
concat' i [TList ls _ _] = do

  disablePact48 <- isExecutionFlagSet FlagDisablePact48
  fixupDivByZero <- not <$> isExecutionFlagSet FlagDisablePact410
  let concatGasCost =
        if disablePact48
        then
          -- Prior to pact-4.8, gas cost is proportional to the number of
          -- strings being concatenated.
          GMakeList $ fromIntegral $ V.length ls
        else
          -- Beginning with pact-4.8, gas cost in proportional to the number of
          -- characters being concatinated and the length of the list.
          let nChars = sum $ termLen <$> ls
                where
                  termLen t = case t of
                    TLitString s -> T.length s
                    _ -> 0
              nStrings = V.length ls
          in
          GTextConcatenation nChars nStrings fixupDivByZero
  computeGas' i concatGasCost $
    let
      ls' = V.toList ls
      concatTextList = flip TLiteral def . LString . T.concat
    in fmap concatTextList $ forM ls' $ \case
      TLitString s -> return s
      t -> isOffChainForkedError FlagDisablePact47 >>= \case
        OffChainError -> evalError' i $ "concat: expecting list of strings: " <> pretty t
        OnChainError -> evalError' i $ "concat: expected list of strings, received value of type: " <> pretty (typeof' t)
concat' i as = argsError i as

-- | Converts a string to a vector of single character strings
-- Ex. "kda" -> [ "k", "d", "a"]
stringToCharList :: Text -> V.Vector (Term a)
stringToCharList t = V.fromList $ tLit . LString . T.singleton <$> T.unpack t

strToList :: GasRNativeFun e
strToList i [TLitString s] = do
  let len = fromIntegral $ T.length s
  ts <- ifExecutionFlagSet' FlagDisablePact48 Pact43IntThreshold Pact48IntThreshold
  ga <- ifExecutionFlagSet' FlagDisablePact45 (GMakeList len) (GMakeList2 len Nothing ts)
  computeGas' i ga $ return $ toTListV tTyString def $ stringToCharList s
strToList i as = argsError i as

strToInt :: RNativeFun e
strToInt i as =
  case as of
    [s'@(TLitString s)] -> checkLen s' s >> doBase s' 10 s
    [b'@(TLitInteger base), s'@(TLitString s)] -> checkLen s' s >> go b' s' base s
    _ -> argsError i as
  where
    checkLen si txt = unless (T.length txt <= 512) $
      evalError' si $ "Invalid input, only up to 512 length supported"
    go bi si base txt
      | base == 64 = doBase64 si txt
      | base >= 2 && base <= 16 = doBase si base txt
      | otherwise = evalError' bi $ "Base value must be >= 2 and <= 16, or 64"
    doBase si base txt = case baseStrToInt base txt of
      Left e -> evalError' si (pretty e)
      Right n -> return (toTerm n)
    doBase64 si txt = do
      parseResult <- base64DecodeWithShimmedErrors (getInfo si) txt
      case parseResult of
        Left e -> evalError' si (pretty (T.pack e))
        Right bs -> return $ toTerm $ bsToInteger bs

bsToInteger :: BS.ByteString -> Integer
bsToInteger bs = fst $ foldl' go (0,(BS.length bs - 1) * 8) $ BS.unpack bs
  where
    go (i,p) w = (i .|. (shift (fromIntegral w) p),p - 8)

integerToBS :: Integer -> BS.ByteString
integerToBS v = BS.pack $ reverse $ go v
  where
    go i | i <= 0xff = [fromIntegral i]
         | otherwise = (fromIntegral (i .&. 0xff)):go (shift i (-8))


txHash :: RNativeFun e
txHash _ [] = (tStr . asString) <$> view eeHash
txHash i as = argsError i as

-- | Change of base for Text-based representations of integrals. Only bases
-- 2 through 16 are supported, for non-empty text of length <= 128
--
-- e.g.
--   -- hexadecimal to decimal
--   baseStrToInt 10 "abcdef123456" = 188900967593046
--
baseStrToInt :: Integer -> Text -> Either Text Integer
baseStrToInt base t =
  if base <= 1 || base > 16
  then Left $ "unsupported base: " `T.append` asString base
  else
    if T.null t
    then Left $ "empty text: " `T.append` asString t
    else foldM go 0 $ T.unpack t
  where
    go :: Integer -> Char -> Either Text Integer
    go acc c' =
      let val = fromIntegral . Char.digitToInt $ c'
      in if val < base
         then pure $ base * acc + val
         else Left $ "character '" <> T.singleton c' <>
                "' is out of range for base " <> tShow base <> ": " <> t
{-# INLINABLE baseStrToInt #-}

base64Encode :: NativeDef
base64Encode = defRNative "base64-encode" go
  (funType tTyString [("string", tTyString)])
  ["(base64-encode \"hello world!\")"]
  "Encode STRING as unpadded base64"
  where
    go :: RNativeFun e
    go i as = case as of
      [TLitString s] ->
        return . tStr $ toB64UrlUnpaddedText $ T.encodeUtf8 s
      _ -> argsError i as

base64decode :: NativeDef
base64decode = defRNative "base64-decode" go
  (funType tTyString [("string", tTyString)])
  ["(base64-decode \"aGVsbG8gd29ybGQh\")"]
  "Decode STRING from unpadded base64"
  where
    go :: RNativeFun e
    go i as = case as of
      [TLitString s] -> do
        parseResult <- base64DecodeWithShimmedErrors (getInfo i) s
        let
          parseResultErrorContext = first ("Could not decode string: " <>) $ parseResult
        case parseResultErrorContext of
          Right bs -> case T.decodeUtf8' bs of
            Right t -> return $ tStr t
            Left _unicodeError -> evalError' i $ "Could not decode string: Base64URL decode failed: invalid unicode"
          Left base64Error -> evalError' i (pretty (T.pack base64Error))
      _ -> argsError i as

-- | Continue a nested defpact.
--   We get the PactId of the nested defpact from the resolved TDef as a qualified name concatenated with
--   the pactId of the parent.
continueNested :: NativeFun e
continueNested i as = gasUnreduced i as $ case as of
  [TApp (App t args _) _] -> lookup' t >>= \d ->
    (,) <$> view eePactStep <*> use evalPactExec >>= \case
      (Just ps, Just pe) -> do
        contArgs <- traverse reduce args >>= traverse enforcePactValue
        let childName = QName (QualifiedName (_dModule d) (asString (_dDefName d)) def)
            cont = PactContinuation childName (stripPactValueInfo <$> contArgs)
        newPactId <- createNestedPactId i cont (_psPactId ps)
        let newPs = PactStep (_psStep ps) (_psRollback ps) newPactId
        case _peNested pe ^. at newPactId of
          Just npe -> resumeNestedPactExec (getInfo i) d (newPs (_npeYield npe)) npe
          Nothing -> evalError' i $ "Attempting to continue a pact that was not nested: " <> pretty d
      _ -> evalError' i "Not within pact invocation"
  _ -> argsError' i as
  where

  lookup' (unTVar -> t) = case t of
    TDef d _ -> pure d
    TVar (Direct (TVar (FQName fq) _)) _ ->
      lookupFreeVar i fq >>= \case
        Ref (TDef d _) -> pure d
        _ -> evalError' i $ "continue: " <> pretty fq <> " is not a defpact"
    TDynamic tref tmem ti -> reduceDynamic tref tmem ti >>= \case
      Right d -> pure d
      Left _ -> evalError' i $ "continue: dynamic reference did not point to Defpact"
    -- Note, pretty on `t` is not dangerous here, as it is not a reduced term.
    _ -> evalError' i $ "continue: argument must be a defpact " <> pretty t
  unTVar = \case
    TVar (Ref d) _ -> unTVar d
    d -> d

-- | A tag for determining how to proceed when encountering an invalid
-- base64-encoded message.
--
-- Legacy:
--   Although we are using base64-bytestring > 1.0, emulate the behavior of
--   base64-bytestring-0.1, for hash compatibility with historical blocks.
--   Messages that continue to fail with new error messages will have those
--   error messages parsed and reformatted into the older form. And messages
--   that now fail due to "non-canonical encoding" will be parsed again
--   leniently, because the legacy base64 parser accepted these messages.
--
-- Simplified:
--    Only base64-encoded messages that pass strict parsing will be accepted
--    (no second lenient pass for non-canonical encodings). All failures to
--    parse will result in the same single error message. This makes the error
--    messages less informative, but makes it easier to maintain compatibility
--    as the base64 parsing algorithm evolves, for failures encountered after
--    the fork that enables simplified error messages.
data Base64DecodingBehavior
  = Legacy
  | Simplified
  deriving (Eq, Show)

-- | Convert from base64-bytestring-1.0 behavior to
-- base64-bytestring-0.1 behavior. This is needed in order
-- to preserve hash equality with old versions of pact (which
-- used base64-bytestring-0.1).
--
-- Throws a Pact `evalError` if it fails to parse an encountered
-- base64-decoding error message.
base64DecodeWithShimmedErrors
  :: Info
  -> Text
  -> Eval e (Either String BS.ByteString)
base64DecodeWithShimmedErrors i txt = do

  -- Use Legacy error behavior when 4.9 is disabled.
  behavior <- ifExecutionFlagSet' FlagDisablePact49 Legacy Simplified

  -- Attempt to decode the bytestring, and convert error messages to Text.
  case first Text.pack $ parseB64UrlUnpaddedText' txt of

    -- base64-bytestring-0.1 is more strict than base64-bytestring-0.1,
    -- so all new successful decodings succeeded on the old version, too.
    Right e -> return $ Right e

    -- With Simplified error messages, map every error to a single string.
    Left _ | behavior == Simplified ->
      return $ Left "Could not base64-decode string"

    -- All cases beyond this point are errors and the behavior context is Legacy.

    -- base64-bytestring-1.0 fails with a "non-canonical encoding" error
    -- for a subset of encoded messages that decode to some bytestring
    -- that would subsequently encode to something other than the original.
    Left e | "non-canonical" `T.isInfixOf` e ->
      return $ Right (B64.decodeLenient (T.encodeUtf8 txt))

    -- This particular error message is reported differently between the
    -- two versions.
    Left "Base64URL decode failed: Base64-encoded bytestring has invalid size" ->
      return $ Left "Base64URL decode failed: invalid base64 encoding near offset 0"

    -- The "invalid character at offset: $n" message is spelled as
    -- "invalid base64 encoding near offset $n" in the old base64-bytestring.
    Left (Text.stripPrefix "Base64URL decode failed: invalid character at offset: " -> Just suffix) -> do
      offset <- adjustedOffset suffix
      return . Left $
        "Base64URL decode failed: invalid base64 encoding near offset " ++ show offset

    Left (Text.stripPrefix "Base64URL decode failed: invalid padding at offset: " -> Just suffix) -> do
      offset <- adjustedOffset suffix
      return . Left $ "Base64URL decode failed: invalid padding near offset " ++ show offset

    -- All other error messages should be the same between old and
    -- new versions of base64-bytestring.
    Left e -> return $ Left (Text.unpack e)
  where
    endsInThreeEquals =
      T.drop (T.length txt - 3) txt == "==="
    paddingAdjustment = if endsInThreeEquals then -1 else 0

    adjustedOffset :: Text -> Eval e Int
    adjustedOffset suffix = case readMaybe (Text.unpack suffix) of
      Just (offsetI :: Int) ->
        return $ offsetI - (offsetI `rem` 4) + paddingAdjustment
      Nothing ->
        evalError i "Could not parse error message"

poseidonHackAChainDefs :: NativeModule
poseidonHackAChainDefs = ("Poseidon Hash", [ poseidonHackAChainDef ])

poseidonHackAChainDef :: NativeDef
poseidonHackAChainDef = defGasRNative
  "poseidon-hash-hack-a-chain"
  poseidon'
  (funType tTyInteger [("i", tTyInteger), ("j", tTyInteger), ("k", tTyInteger), ("l", tTyInteger), ("m", tTyInteger), ("n", tTyInteger), ("o", tTyInteger), ("p", tTyInteger)])
    ["(poseidon-hash-hack-a-chain 1)"
    ,"(poseidon-hash-hack-a-chain 1 2)"
    ,"(poseidon-hash-hack-a-chain 1 2 3 4 5 6)"
    ,"(poseidon-hash-hack-a-chain 1 2 3 4 5 6 7 8)"
    ]
    "Poseidon Hash Function. Note: This is a reference version of the Poseidon hash function used by Hack-a-Chain."
  where
  poseidon' :: RNativeFun e
  poseidon' i as
    | not (null as) && length as <= 8,
      Just intArgs <- traverse (preview _TLitInteger) as
      = computeGas' i (GPoseidonHashHackAChain $ length as) $
        return $ toTerm $ poseidon intArgs
     | otherwise = argsError i as

hyperlaneDefs :: NativeModule
hyperlaneDefs = ("Hyperlane",)
  [ hyperlaneMessageIdDef
  , hyperlaneDecodeTokenMessageDef
  ]

hyperlaneMessageIdDef :: NativeDef
hyperlaneMessageIdDef = defGasRNative
  "hyperlane-message-id"
  hyperlaneMessageId'
  (funType tTyString [("x", tTyObjectAny)])
  [
    "(hyperlane-message-id {\"destinationDomain\": 1,\"nonce\": 325,\"originDomain\": 626,\"recipient\": \"0x71C7656EC7ab88b098defB751B7401B5f6d8976F\",\"sender\": \"0x6b622d746f6b656e2d726f75746572\",\"tokenMessage\": {\"amount\": 10000000000000000000.0,\"recipient\": \"0x71C7656EC7ab88b098defB751B7401B5f6d8976F\"},\"version\": 1})"
  ]
  "Get the Message Id of a Hyperlane Message object."
  where
    hyperlaneMessageId' :: RNativeFun e
    hyperlaneMessageId' i args = case args of
      [TObject o _] ->
        computeGas' i (GHyperlaneMessageId (BS.length (getTokenRecipient o)))
        $ return $ toTerm $ hyperlaneMessageId o
      _ -> argsError i args

    getTokenRecipient :: Object n -> BS.ByteString
    getTokenRecipient o =
      let mRecipient = do
            let om = _objectMap (_oObject o)
            tokenObject <- om ^? at "tokenMessage" . _Just . _TObject . _1
            let tm = _objectMap (_oObject tokenObject)
            tm ^? at "recipient" . _Just . _TLiteral . _1 . _LString
      in
      case mRecipient of
        Nothing -> error "couldn't decode token recipient"
        Just t -> T.encodeUtf8 t

hyperlaneDecodeTokenMessageDef :: NativeDef
hyperlaneDecodeTokenMessageDef =
  defGasRNative
    "hyperlane-decode-tokenmessage"
    hyperlaneDecodeTokenMessageDef'
    (funType tTyObjectAny [("x", tTyString)])
    ["(TODO example)"]
    "Decode a base-64 encoded Hyperlane Token Message into an object `{recipient:STRING, amount:DECIMAL, chainId:STRING}`."
  where
    hyperlaneDecodeTokenMessageDef' :: RNativeFun e
    hyperlaneDecodeTokenMessageDef' i args = case args of

      [TLitString msg] ->
        -- We do not need to handle historical b64 error message shimming
        -- or decoding from non-canonical strings in this base-64 decoder,
        -- because this native is added in a Pact version that latre than when
        -- we moved to base64-bytestring >= 1.0, which behaves succeeds and
        -- fails in exactly the cases we expect.
        -- (The only change we make to its output is to strip error messages).
        -- TODO: standard alphabet, or URL?
        computeGas' i (GHyperlaneDecodeTokenMessage (T.length msg)) $
          case B64URL.decode (T.encodeUtf8 msg) of
            Left _ -> evalError' i "Failed to base64-decode token message"
            Right bytes -> do
              case runGetOrFail (getTokenMessageERC20 <* eof) (BS.fromStrict bytes) of
                -- In case of Binary decoding failure, emit a terse error message.
                -- If the error message begins with TokenError, we know that we
                -- created it, and it is going to be stable (non-forking).
                -- If it does not start with TokenMessage, it may have come from
                -- the Binary library, and we will suppress it to shield ourselves
                -- from forking behavior if we update our Binary version.
                -- (TODO: Do we suppress error messages on-chain anyway?)
                Left (_,_,e) | "TokenMessage" `isPrefixOf` e -> evalError' i $ "Decoding error: " <> pretty e
                Left _ -> evalError' i "Decoding error: binary decoding failed" 
                -- TODO: Do we need to assert that the bytes are fully consumed
                -- by parsing?
                -- TODO: Is this format correct? I.e. field names?
                Right (_,_,(amount, chain, recipient)) ->
                  case PGuard <$> J.eitherDecode (BS.fromStrict  $ T.encodeUtf8 recipient) of
                    Left _ -> evalError' i $ "Could not parse recipient into a guard"
                    Right g ->
                      pure $ toTObject TyAny def
                        [("recipient", fromPactValue g)
                        ,("amount", TLiteral (LDecimal $ wordToDecimal amount) def)
                        ,("chainId", toTerm chain)
                        ]
      _ -> argsError i args

    -- The TokenMessage contains a recipient (text) and an amount (word-256).
    getTokenMessageERC20 :: Get (Word256, ChainId, Text)
    getTokenMessageERC20 = do

      -- Parse the size of the following amount field.
      amountSize <- fromIntegral @Word256 @Int <$> getWord256be
      unless (amountSize == 96)
        (fail $ "TokenMessage amountSize expected 96, found " ++ show amountSize)
      tmAmount <- getWord256be
      tmChainId <- getWord256be

      recipientSize <- getWord256be
      tmRecipient <- T.decodeUtf8 <$> getRecipient recipientSize

      return (tmAmount, ChainId { _chainId = T.pack (show (toInteger tmChainId))}, tmRecipient)
      where
        getWord256be = get @Word256

        -- TODO: We check the size. Is this ok?
        -- | Reads a given number of bytes and the rest because binary data padded up to 32 bytes.
        getRecipient :: Word256 -> Get BS.ByteString
        getRecipient size = do
          recipient <- BS.take (fromIntegral size) <$> getByteString (fromIntegral $ size + restSize size)
          if BS.length recipient < fromIntegral size
            then fail "TokenMessage recipient was smaller than expected"
            else pure recipient


    wordToDecimal :: Word256 -> Decimal
    wordToDecimal w =
      let ethInWei = 1000000000000000000 -- 1e18
      in fromRational (toInteger w % ethInWei)

    eof :: Get ()
    eof = do
      done <- isEmpty
      unless done $ fail "pending bytes in input"

-- | Helper function for creating TokenMessages encoded in the ERC20 format
--   and base64url encoded. Used for generating test data.
encodeTokenMessage :: BS.ByteString -> Word256 -> Word256 -> Text
encodeTokenMessage recipient amount chain = T.decodeUtf8 $ B64URL.encode (BS.toStrict bytes)
  where
    bytes = runPut $ do
      putWord256be (96 :: Word256)
      putWord256be amount
      putWord256be chain
      putWord256be recipientSize
      putByteString recipientBytes

    (recipientBytes, recipientSize) = padRight recipient

    putWord256be :: Word256 -> Put
    putWord256be  = put @Word256

padRight :: BS.ByteString -> (BS.ByteString, Word256)
padRight s =
  let
    size = BS.length s
    missingZeroes = restSize size
  in (s <> BS.replicate missingZeroes 0, fromIntegral size)

-- | Returns the modular of 32 bytes.
restSize :: Integral a => a -> a
restSize size = (32 - size) `mod` 32