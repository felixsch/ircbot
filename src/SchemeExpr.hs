{-# LANGUAGE OverloadedStrings #-}

module SchemeExpr 
 ( Symbol
 , Expr(..)
 , showType
 , parseExpr
 ) where


import Control.Monad
import Control.Applicative

import qualified Data.ByteString.Char8 as B
import Data.Attoparsec.ByteString.Char8

type Symbol = B.ByteString

data Expr = SString   B.ByteString
          | SBool     Bool
          | SInt      Int
          | SList     [Expr]
          | SSymbol   Symbol
          | SFunction Symbol
          | SBind     Symbol
          | SNil

instance Show Expr where
    show (SString b) = B.unpack b
    show (SBool   b) = show b
    show (SInt    i) = show i
    show (SSymbol s) = B.unpack s
    show (SList   l) = "(" ++ unwords (map show l) ++ ")"
    show (SNil     ) = "Nil"

showType :: Expr -> B.ByteString
showType (SString _) = "String"
showType (SBool   _) = "Bool"
showType (SInt    _) = "Int"
showType (SSymbol _) = "Symbol"
showType (SList   _) = "List"
showType (SNil     ) = "Nil"


parseExpr :: B.ByteString -> Maybe Expr
parseExpr = from . parseOnly pList
    where
      from (Left  _) = Nothing
      from (Right x) = Just x

wss :: Parser ()
wss = void $ many (char ' ')

comment :: Parser ()
comment = void $ char ';' *> many anyChar 

pInt :: Parser Expr
pInt = SInt <$> signed decimal


pBool :: Parser Expr
pBool = SBool <$> choice [ string "#t" *> return True,
                           string "#f" *> return False ]

pString :: Parser Expr
pString = SString <$ char '"' <*> takeTill (== '"') <* char '"'

pSymbol :: Parser Expr
pSymbol = SSymbol <$> liftA2 B.cons start name
    where
        symbol  = satisfy $ inClass "-+=!|&/*<>"
        start   = letter_ascii <|> symbol
        name    = B.pack <$> many (letter_ascii <|> symbol <|> digit)

pList :: Parser Expr
pList = SList <$ ignr <* char '(' <*> many expr <* char ')' <* ignr
    where
        ignr  = wss <|> comment
        expr  = wss *> types
        types = pInt <|> pBool <|> pString <|> pSymbol <|> pList
