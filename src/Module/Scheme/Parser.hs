{-# LANGUAGE OverloadedStrings #-}

module Module.Scheme.Parser
 ( parseExpr 
 ) where


import Control.Monad
import Control.Applicative

import qualified Data.Text as T
import Data.Attoparsec.Text

import Module.Scheme.Types

parseExpr :: T.Text -> Maybe Expr
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
pSymbol = SSymbol <$> liftA2 T.cons start name
    where
        symbol  = satisfy $ inClass "-+=!|&/*<>"
        start   = letter <|> symbol
        name    = T.pack <$> many (letter <|> symbol <|> digit)

pList :: Parser Expr
pList = SList <$ ignr <* char '(' <*> many expr <* char ')' <* ignr
    where
        ignr  = wss <|> comment
        expr  = wss *> pExpr

pQuote :: Parser Expr 
pQuote = do
    char '\''
    p <- pList
    return $ SList [SSymbol "quote", p]

pExpr :: Parser Expr
pExpr = pInt <|> pBool <|> pQuote <|> pString <|> pSymbol <|> pList
