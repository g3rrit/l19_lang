module Syntax where

import Data.Maybe

data Definition
  = Struct_Def Struct
  | Function_Def Function
  deriving (Show)

data Struct = Struct {
  sid :: String,
  mem :: [Var]}
  deriving (Show)

data Function = Function {
  fid  :: String,
  rt   :: PType,
  args :: [Var],
  body :: [Stm]}
  deriving (Show)

data Var = Var {
  id :: String,
  t  :: PType}
  deriving (Show)

data PType
  = Pointer PType
  | U8
  | I8
  | U16
  | I16
  | U32
  | I32
  | U64
  | I64
  | F32
  | F64
  | VOID
  | Id_Type String
  | Function_Type {
    fargs :: [PType],
    frt   :: PType}
  deriving (Show)

data Stm
  = Ret_Stm (Maybe Exp)
  | Brk_Stm
  | Con_Stm
  | Var_Stm Var
  | Conditional_Stm (Maybe Exp) [Stm] (Maybe Stm)
  | Loop_Stm Exp [Stm]
  deriving (Show)

data Exp
  = Integer_L Integer
  | Float_L Double
  | Acc_Dot_Exp Exp Exp
  | Acc_Arr_Exp Exp Exp
  | Cast_Exp PType Exp
  | Id_Exp String
  | Ref_Exp Exp
  | Deref_Exp Exp
  deriving (Show)
