module Syntax where

import Data.Maby

data Definition
  = Struct_Def Struct
  | Function_Def Function
  deriving (Show)

data Struct = Struct {
  id  :: String,
  mem :: [Var]}
  deriving (Show)

data Function = Function {
  id   :: String,
  t    :: PType,
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
  deriving (Show)

data Stm
  = Ret_Stm (Maby Exp)
  | Brk_Stm
  | Con_Stm
  | Var_Stm Var
  | Conditional_Stm (Maby Exp) [Stm] (Maby Stm)
  | Loop_Stm Exp [Stm]
  deriving (Show)

data Exp
  = Integer Int
  | Float Double
  | Acc_Dot_Exp Exp Exp
  | Acc_Arr_Exp Exp Exp
  | Cast_Exp PType Exp
  | Id_Exp String
  | Ref_Exp Exp
  | Deref_Exp Exp
  deriving (Show)

data Var = Var String PType
  deriving (Show)
