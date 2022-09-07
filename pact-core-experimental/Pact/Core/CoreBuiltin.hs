module Pact.Core.CoreBuiltin where



-- monomorphised builtin operations
-- TODO: TIME
data CoreBuiltin
  -- IntOps
  -- Integer Add
  = AddInt
  -- Int Num functions
  | SubInt
  | DivInt
  | MulInt
  | NegateInt
  | AbsInt
  -- Int fractional
  | ExpInt
  | LnInt
  | SqrtInt
  | LogBaseInt
  -- General int ops
  | ModInt
  | BitAndInt
  | BitOrInt
  | BitXorInt
  | BitShiftInt
  | BitComplementInt
  -- Int show instance
  | ShowInt
  -- Int Equality
  | EqInt
  | NeqInt
  | GTInt
  | GEQInt
  | LTInt
  | LEQInt
  -- If
  | IfElse
  -- Decimal ops
  -- Decimal add
  | AddDec
  -- Decimal num
  | SubDec
  | DivDec
  | MulDec
  | NegateDec
  | AbsDec
  -- Decimal rounding ops
  | RoundDec
  | CeilingDec
  | FloorDec
  -- Decimal rounding ops
  | ExpDec
  | LnDec
  | LogBaseDec
  | SqrtDec
  -- Decimal Show
  | ShowDec
  -- Decimal Equality
  | EqDec
  | NeqDec
  -- Decimal ord
  | GTDec
  | GEQDec
  | LTDec
  | LEQDec
  -- Bool Comparisons
  | AndBool
  | OrBool
  | NotBool
  -- other bool ops
  | EqBool
  | NeqBool
  | ShowBool
  -- String Equality
  | EqStr
  | NeqStr
  -- String Ord
  | GTStr
  | GEQStr
  | LTStr
  | LEQStr
   -- String Add
  | AddStr
  -- String ListLike
  | ConcatStr
  | DropStr
  | TakeStr
  | LengthStr
  | ReverseStr
  -- String Show
  | ShowStr
  -- Object equality
  | EqObj
  | NeqObj
  -- List Equality
  | EqList
  | NeqList
  -- List Ord
  | GTList
  | GEQList
  | LTList
  | LEQList
  -- List Show
  | ShowList
  -- List Add
  | AddList
  -- ListLike List
  | TakeList
  | DropList
  | LengthList
  | ConcatList
  | ReverseList
  -- Misc list ops
  | FilterList
  | DistinctList
  | MapList
  | ZipList
  | FoldList
  -- Unit ops
  | EqUnit
  | NeqUnit
  | ShowUnit
  -- Others
  | Enforce
  | EnforceOne
  | Enumerate
  | EnumerateStepN
  | ReadInteger
  | ReadDecimal
  | ReadString
  | ReadKeyset
  | EnforceGuard
  | KeysetRefGuard
  | CreateUserGuard
  | ListAccess
  | B64Encode
  | B64Decode
  deriving (Eq, Show, Ord, Bounded, Enum)


instance Pretty CoreBuiltin where
  pretty = pretty . coreBuiltinToText



instance BuiltinArity CoreBuiltin where
  builtinArity = \case
    AddInt -> 2
    SubInt -> 2
    DivInt -> 2
    MulInt -> 2
    NegateInt -> 1
    AbsInt -> 1
    ExpInt -> 1
    LnInt -> 1
    SqrtInt -> 1
    LogBaseInt -> 2
    ModInt -> 2
    BitAndInt -> 2
    BitOrInt -> 2
    BitXorInt -> 2
    BitShiftInt -> 2
    BitComplementInt -> 1
    ShowInt -> 1
    EqInt -> 2
    NeqInt -> 2
    GTInt -> 2
    GEQInt -> 2
    LTInt -> 2
    LEQInt -> 2
    IfElse -> 3
    AddDec -> 2
    SubDec -> 2
    DivDec -> 2
    MulDec -> 2
    NegateDec -> 1
    AbsDec -> 1
    RoundDec -> 1
    CeilingDec -> 1
    FloorDec -> 1
    ExpDec -> 1
    LnDec -> 1
    LogBaseDec -> 2
    SqrtDec -> 1
    ShowDec -> 1
    EqDec -> 2
    NeqDec -> 2
    GTDec -> 2
    GEQDec -> 2
    LTDec -> 2
    LEQDec -> 2
    AndBool -> 2
    OrBool -> 2
    NotBool -> 2
    EqBool -> 2
    NeqBool -> 2
    ShowBool -> 1
    EqStr -> 2
    NeqStr -> 2
    GTStr -> 2
    GEQStr -> 2
    LTStr -> 2
    LEQStr -> 2
    AddStr -> 2
    ConcatStr -> 1
    DropStr -> 2
    TakeStr -> 2
    LengthStr -> 1
    ReverseStr -> 1
    ShowStr -> 1
    EqObj -> 2
    NeqObj -> 2
    EqList -> 2
    NeqList -> 2
    GTList -> 2
    GEQList -> 2
    LTList -> 2
    LEQList -> 2
    ShowList -> 1
    AddList -> 2
    TakeList -> 2
    DropList -> 2
    LengthList -> 1
    ConcatList -> 1
    ReverseList -> 1
    FilterList -> 2
    DistinctList -> 1
    MapList -> 2
    ZipList -> 3
    FoldList -> 3
    EqUnit -> 2
    NeqUnit -> 2
    ShowUnit -> 1
    Enforce -> 2
    EnforceOne -> 2
    Enumerate -> 2
    EnumerateStepN -> 3
    ReadInteger -> 1
    ReadDecimal -> 1
    ReadString -> 1
    ReadKeyset -> 1
    EnforceGuard -> 1
    KeysetRefGuard -> 1
    CreateUserGuard -> 1
    ListAccess -> 2
    B64Encode -> 1
    B64Decode -> 1

coreBuiltinToText :: CoreBuiltin -> Text
coreBuiltinToText = \case
-- IntOps
  AddInt -> "addInt"
  -- Int Num functions
  SubInt -> "subInt"
  DivInt -> "divInt"
  MulInt -> "mulInt"
  NegateInt -> "negateInt"
  AbsInt -> "absInt"
  -- Int fractional
  ExpInt -> "expInt"
  LnInt -> "lnInt"
  SqrtInt -> "sqrtInt"
  LogBaseInt -> "logBaseInt"
  -- General int ops
  ModInt -> "modInt"
  BitAndInt -> "bitAndInt"
  BitOrInt -> "bitOrInt"
  BitXorInt -> "bitXorInt"
  BitShiftInt -> "bitShiftInt"
  BitComplementInt -> "bitComplementInt"
  -- Int show instance
  ShowInt -> "showInt"
  -- Int Equality
  EqInt -> "eqInt"
  NeqInt -> "neqInt"
  GTInt -> "gtInt"
  GEQInt -> "geqInt"
  LTInt -> "ltInt"
  LEQInt -> "leqInt"
  -- If
  IfElse -> "ifElse"
  -- Decimal ops
  -- Decimal add
  AddDec -> "addDec"
  -- Decimal num
  SubDec -> "subDec"
  DivDec -> "divDec"
  MulDec -> "mulDec"
  NegateDec -> "negateDec"
  AbsDec -> "absDec"
  -- Decimal rounding ops
  RoundDec -> "roundDec"
  CeilingDec -> "ceilingDec"
  FloorDec -> "floorDec"
  -- Decimal rounding ops
  ExpDec -> "expDec"
  LnDec -> "lnDec"
  LogBaseDec -> "logBaseDec"
  SqrtDec -> "sqrtDec"
  -- Decimal Show
  ShowDec -> "showDec"
  -- Decimal Equality
  EqDec -> "eqDec"
  NeqDec -> "neqDec"
  -- Decimal ord
  GTDec -> "gtDec"
  GEQDec -> "geqDec"
  LTDec -> "ltDec"
  LEQDec -> "leqDec"
  -- Bool Comparisons
  AndBool -> "andBool"
  OrBool -> "orBool"
  NotBool -> "notBool"
  -- other bool ops
  EqBool -> "eqBool"
  NeqBool -> "neqBool"
  ShowBool -> "showBool"
  -- String Equality
  EqStr -> "eqStr"
  NeqStr -> "neqStr"
  -- String Ord
  GTStr -> "gtStr"
  GEQStr -> "gtStr"
  LTStr -> "gtStr"
  LEQStr -> "gtStr"
   -- String Add
  AddStr -> "addStr"
  -- String ListLike
  ConcatStr -> "concatStr"
  DropStr -> "dropStr"
  TakeStr -> "takeStr"
  LengthStr -> "lengthStr"
  ReverseStr -> "reverseStr"
  -- String Show
  ShowStr -> "showStr"
  -- Object equality
  EqObj -> "eqObj"
  NeqObj -> "neqObj"
  -- List Equality
  EqList -> "eqList"
  NeqList -> "neqList"
  -- List Ord
  GTList -> "gtList"
  GEQList -> "geqList"
  LTList -> "ltList"
  LEQList -> "leqList"
  -- List Show
  ShowList -> "showList"
  -- List Add
  AddList -> "addList"
  -- ListLike List
  TakeList -> "takeList"
  DropList -> "dropList"
  LengthList -> "lengthList"
  ConcatList -> "concatList"
  ReverseList -> "reverseList"
  -- Misc list ops
  FilterList -> "filterList"
  DistinctList -> "distinctList"
  MapList -> "mapList"
  ZipList -> "zipList"
  FoldList -> "foldList"
  -- Unit ops
  EqUnit -> "eqUnit"
  NeqUnit -> "neqUnit"
  ShowUnit -> "showUnit"
  -- Others
  Enforce -> "enforce"
  EnforceOne -> "enforceOn"
  Enumerate -> "enumerate"
  EnumerateStepN -> "enumerateStep"
  ReadInteger -> "read-integer"
  ReadDecimal -> "read-decimal"
  ReadString -> "read-string"
  ReadKeyset -> "read-keyset"
  EnforceGuard -> "enforce-guard"
  KeysetRefGuard -> "keyset-ref-guard"
  CreateUserGuard -> "create-user-guard"
  ListAccess -> "at"
  B64Encode -> "base64-encode"
  B64Decode -> "base64-decode"
