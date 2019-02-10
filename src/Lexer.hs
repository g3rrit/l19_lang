module Lexer where

import Text.Parsec.String (Parser)
import Text.Parsec.Language (emptyDef)

import qualified Text.Parsec.Token as Tok

lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser style
  where
    ops = [ "*", "&", ".", "->", ":", "="]
    names = [ "if", "el", "brk", "con", "while",
              "void", "u8", "i8", "u16", "i16", "u32", "i32",
              "u64", "i64", "f32", "f64", "ret"]
    special_id = oneOf "_+-?!"
    style = emptyDef {
        Tok.commentStart = "/*"
      , Tok.commentEnd = "*/"
      , Tok.identStart = letter <|> special_id
      , Tok.identLetter = alphaNum <|> special_id
      , Tok.commentLine = "//"
      , Tok.reservedOpNames = ops
      , Tok.reservedNames = names
      }

integer :: Parser Integer
integer = Tok.integer lexer

float :: Parser Double
float = Tok.float lexer

parens :: Parser a -> Parser a
parens = Tok.parens lexer

braces :: Parser a -> Parser a
braces = Tok.braces lexer

angles :: Parser a -> Parser a
angles = Tok.angles lexer

squares :: Parser a -> Parser a
squares = Tok.squares lexer

comma :: Parser a -> Parser [a]
comma = Tok.commaSep lexer

identifier :: Parser String
identifier = Tok.identifier lexer

reserved :: String -> Parser ()
reserved = Tok.reserved lexer

reserved_op = Tok.reservedOp lexer
