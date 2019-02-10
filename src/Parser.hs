module Parser where

import Text.Parsec
import Text.Parsec.String (Parser)

import qualified Text.Parsec.Expr as Ex
import qualified Text.Parsec.Token as Tok

import Lexer
import Syntax

prog :: Parser [Definition]
prog = many $ definition

contents :: Parser a -> Parser a
contents p = do
  Tok.whiteSpace lexer
  r <- p
  eof
  return r

parse_prog :: String -> Either ParseError [Definition]
parse_prog s = parse (contents prog) "<error>" s

---------------------------------------
-- DEFINITION
---------------------------------------

definition :: Parser Definition
definition = struct_def
         <|> function_def

struct_def :: Parser Definition
struct_def = do
  s <- struct
  return $ Struct_Def s
  
function_def :: Parser Definition
function_def = do
  f <- function
  return $ Function_Def

---------------------------------------
-- STRUCT
---------------------------------------

struct :: Parser Struct
struct = do
  id  <- identifier
  mem <- braces $ comma var
  return $ Struct id mem

---------------------------------------
-- FUNCTION
---------------------------------------

function :: Parser Function
function = do
  id <- identifier
  args <- parens $ comma var
  reserved_op ":"
  t <- ptype
  body <-  braces $ many stm
  return $ Function id t args body

---------------------------------------
-- VAR
---------------------------------------
  
var :: Parser Var
var = do
  id <- identifier
  reserved_op ":"
  t  <- ptype
  return $ Var id t

---------------------------------------
-- TYPE
---------------------------------------

ptype :: Parser PType
ptype = pointer_type
    <|> do reserved "u8"
           return U8
    <|> do reserved "i8"
           return I8
    <|> do reserved "u16"
           return U16
    <|> do reserved "i32"
           return I32
    <|> do reserved "u32"
           return U32
    <|> do reserved "i64"
           return I64
    <|> do reserved "f32"
           return F32
    <|> do reserved "f64"
           return F64
    <|> do reserved "void"
           return VOID
    <|> do
           i <- identifier
           return $ Id_Type i

---------------------------------------
-- STATEMENT
---------------------------------------

stm :: Parser Stm
stm = ret_stm
  <|> brk_stm
  <|> con_stm
  <|> loop_stm
  <|> conditional_stm
  <|> var_stm

ret_stm :: Parser Stm
ret_stm = do
  reserved "ret"
  r <- parens $ optionMaby exp
  return $ Ret_Stm r

brk_stm :: Parser Stm
brk_stm = do
  reserved "brk"
  return $ Brk_Stm

con_stm :: Parser Stm
con_stm = do
  reserved "con"
  return $ Con_Stm

loop_stm :: Parser Stm
loop_stm = do
  reserved "while"
  con  <- exp
  body <- braces $ many stm
  return $ Loop_Stm con body

conditional_stm :: Parser Stm
conditional_stm = do
  reserved "if"
  con  <- exp
  body <- braces $ many stm
  next <- optionMaby conditional_stm_opt
  return $ Conditional_Stm (Just con) body next

conditional_stm_opt :: Parser Stm
conditional_stm_opt = do
  reserved "el"
  con  <- optionMaby exp
  body <- braces $ many stm
  next <- optionMaby conditional_stm_opt
  return $ Conditional_Stm con body next

var_stm :: Parser Stm
var_stm = do
  v <- var
  return $ Var_Stm v

---------------------------------------
-- EXPRESSION
---------------------------------------

exp :: Parser Exp
exp = int_exp
  <|> float_exp
  <|> try acc_dot_exp
  <|> try acc_arr_exp
  <|> cast_exp
  <|> id_exp
  <|> ref_exp
  <|> deref_exp

int_exp :: Parser Exp
int_exp = do
  i <- integer
  return $ Integer i

float_exp :: Parser Exp
float_exp = do
  f <- float
  return $ Float f

acc_dot_exp :: Parser Exp
acc_dot_exp = do
  i <- id_exp
  reserved_op "."
  a <- (acc_dot_exp <|> id_exp)
  return $ Acc_Dot_Exp i a

acc_arr_exp :: Parser Exp
acc_arr_exp = do
  i <- id_exp
  reserved_op "->"
  a <- (acc_dot_exp <|> id_exp)
  return $ Acc_Arr_Exp i a

cast_exp :: Parser Exp
cast_exp = do
  t <- angles ptype
  e <- parens exp
  return $ Cast_Exp t e

id_exp :: Parser Exp
id_exp = do
  i <- identifier
  return $ Id_Exp i

ref_exp :: Parser Exp
ref_exp = do
  reserved_op "*"
  e <- exp
  return $ Ref_Exp e

deref_exp :: Parser Exp
deref_exp = do
  reserved_op "&"
  e <- exp
  return $ Deref_Exp e

