{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Pact.Analyze.Infer where

import Control.Monad.Trans.Reader
import Control.Monad.IO.Class (liftIO)
import Control.Lens hiding ((.=))
import Pact.Typecheck
import Pact.Types
import Data.Either
import Data.Decimal
import Data.Aeson hiding (Object)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Text.PrettyPrint.ANSI.Leijen hiding ((<$>),(<$$>))
import GHC.Generics

import SmtLib.Syntax.Syntax
import SmtLib.Parsers.CommandsParsers (parseCommand)
import Text.Parsec (parse)
import qualified SmtLib.Syntax.Syntax as Smt
import qualified SmtLib.Syntax.ShowSL as SmtShow

import qualified Data.ByteString.Char8 as BS8
import qualified Data.Yaml as Yaml
import qualified Text.Parsec as Parsec
import qualified SmtLib.Parsers.CommandsParsers as SmtParser

-- helper stuff
_parseSmtCmd :: String -> Smt.Command
_parseSmtCmd s = let (Right f) = Parsec.parse SmtParser.parseCommand "" s in f


getSampFunc :: String -> IO (Fun Node)
getSampFunc s = do
  (fn, _) <- _infer "examples/analyze-tests/analyze-tests.repl" "analyze-tests" s
  return fn

--FDefun { _fInfo = "(defun gt-ten (a:integer)"
--       , _fName = "analyze-tests.gt-ten"
--       , _fType = "(a:integer) -> <a>"
--       , _fArgs = ["a"(analyze-tests.gt-ten_a0::integer)]
--       , _fBody =
--         [ App { _aNode = "appNif1::<appNif1_a>"
--               , _aAppFun = FNative
--                   { _fInfo = ""
--                   , _fName = "if"
--                   , _fTypes = "(cond:bool then:<a> else:<a>) -> <a> :| []"
--                   , _fSpecial = Nothing}
--               , _aAppArgs =
--                 [ App { _aNode = appN>2::bool
--                       , _aAppFun = FNative { _fInfo = ""
--                                            , _fName = ">", _fTypes = "(x:<a[integer,decimal,string,time]> y:<a[integer,decimal,string,time]>) -> bool :| []"
--                                            , _fSpecial = Nothing}
--                       , _aAppArgs =
--                         [ Var {_aNode = analyze-tests.gt-ten_a0::integer}
--                         , Prim {_aNode = integer3::integer, _aPrimValue = PrimLit 10}
--                         ]
--                       }
--                 , Prim { _aNode = string4::string
--                        , _aPrimValue = PrimLit "more than ten"}
--                 , Prim {_aNode = string5::string
--                        , _aPrimValue = PrimLit "less than ten"}
--                 ]}
--         ]}

--FDefun
--  { _fInfo =   "(defun simple-let (a:integer)"
--  , _fName = "analyze-tests.simple-let"
--  , _fType = "(a:integer) -> <c>"
--  , _fArgs = ["a"(analyze-tests.simple-let_a0::integer)]
--  , _fBody =
--    [ App { _aNode = appNenforce1::bool
--          , _aAppFun = FNative { _fInfo = ""
--                               , _fName = "enforce"
--                               , _fTypes = "(test:bool msg:string) -> bool :| []"
--                               , _fSpecial = Nothing}
--          , _aAppArgs = [App {_aNode = appN>2::bool
--                             , _aAppFun = FNative {_fInfo = ""
--                                                  , _fName = ">"
--                                                  , _fTypes = "(x:<a[integer,decimal,string,time]> y:<a[integer,decimal,string,time]>) -> bool :| []"
--                                                  , _fSpecial = Nothing}
--                             , _aAppArgs =
--                               [Var {_aNode = analyze-tests.simple-let_a0::integer}
--                               ,Prim {_aNode = integer3::integer, _aPrimValue = PrimLit 0}
--                               ]}
--                        ,Prim {_aNode = string4::string, _aPrimValue = PrimLit "less than 0"}
--                        ]}
--    ,Binding {_aNode = let5::integer
--             , _aBindings =
--               [ ("b"(let5_b6::integer),App { _aNode = appN+7::integer
--                                            , _aAppFun = FNative {_fInfo = ""
--                                                                 , _fName = "+"
--                                                                 , _fTypes = "(x:<a[integer,decimal]> y:<a[integer,decimal]>) -> <a[integer,decimal]> :| [(x:<a[integer,decimal]> y:<b[integer,decimal]>) -> decimal,(x:<a[string,[<l>],object:<{o}>]> y:<a[string,[<l>],object:<{o}>]>) -> <a[string,[<l>],object:<{o}>]>]"
--                                                                 , _fSpecial = Nothing}
--                                            , _aAppArgs =
--                                              [ Var {_aNode = analyze-tests.simple-let_a0::integer}
--                                              , Prim {_aNode = integer8::integer
--                                                     , _aPrimValue = PrimLit 10}
--                                              ]}
--                 )
--               ]
--             , _aBody = [App {_aNode = appNenforce9::bool
--                             , _aAppFun = FNative {_fInfo = ""
--                                                  , _fName = "enforce"
--                                                  , _fTypes = "(test:bool msg:string) -> bool :| []"
--                                                  , _fSpecial = Nothing}
--                             , _aAppArgs =
--                               [ App {_aNode = appN>10::bool
--                                     , _aAppFun = FNative {_fInfo = ""
--                                                          , _fName = ">"
--                                                          , _fTypes = "(x:<a[integer,decimal,string,time]> y:<a[integer,decimal,string,time]>) -> bool :| []"
--                                                          , _fSpecial = Nothing}
--                                     , _aAppArgs =
--                                       [Var {_aNode = analyze-tests.simple-let_a0::integer}
--                                       ,Prim {_aNode = integer11::integer, _aPrimValue = PrimLit 10}]}
--                               , Prim {_aNode = string12::string, _aPrimValue = PrimLit "less than 10"}]}
--                        ,App {_aNode = appNenforce13::bool
--                             , _aAppFun = FNative {_fInfo = ""
--                                                  , _fName = "enforce"
--                                                  , _fTypes = "(test:bool msg:string) -> bool :| []"
--                                                  , _fSpecial = Nothing}
--                             , _aAppArgs =
--                               [ App { _aNode = appN<14::bool
--                                     , _aAppFun = FNative {_fInfo = ""
--                                                          , _fName = "<"
--                                                          , _fTypes = "(x:<a[integer,decimal,string,time]> y:<a[integer,decimal,string,time]>) -> bool :| []"
--                                                          , _fSpecial = Nothing}
--                                     , _aAppArgs =
--                                       [Var {_aNode = let5_b6::integer}
--                                       ,Prim {_aNode = integer15::integer, _aPrimValue = PrimLit 20}
--                                       ]}
--                               ,Prim {_aNode = string16::string, _aPrimValue = PrimLit "greater than 20"}
--                               ]}
--                        ,Var {_aNode = let5_b6::integer}
--                        ]
--             , _aBindType = let}
--    ]}
