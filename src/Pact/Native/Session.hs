-- |

{-# LANGUAGE OverloadedStrings #-}

module Pact.Native.Session (sessionDefs) where

import Data.Default (def)

import Pact.Native.Internal(NativeModule, NativeDef, SpecialForm(WithSession), defNative, funType)
import Pact.Types.Runtime (argsError')
import Pact.Types.Native (NativeFun, specialForm)
import Pact.Types.Term (Example(LitExample), Term(TBinding), BindType(BindSchema))
import Pact.Types.Type (Type(TySchema), SchemaType(TyBinding), mkSchemaVar, mkTyVar)

sessionDefs :: NativeModule
sessionDefs =
  ("Session",[withSessionDef])

withSessionDef :: NativeDef
withSessionDef =
  let
    rt = mkSchemaVar "row"
    a = mkTyVar "a" []
    bindTy = TySchema TyBinding rt def
  in
    defNative (specialForm WithSession) withSession
    (funType a [("bindings", bindTy)])
    [ LitExample "(with-session { \"user\":=username \"user-keyset\":=keyset })"]
    ""

withSession :: NativeFun e
withSession fi as@[b@(TBinding ps bd (BindSchema _) _)] = undefined
withSession fi as = argsError' fi as
