{-# LANGUAGE OverloadedStrings #-}

module Module.Scheme.Types
  ( Symbol
  , Expr (..)
  , showType
  , Call
  , Scope (..)
  , emptyScope
  , Env (..)
  , emptyEnv
  , WithScheme (..)
  , Scheme
  , (...)
  ) where

import Control.Monad.Except

import qualified Data.Text as T

import Network.IRC.Action ( Action )


type Symbol = T.Text

data Expr = SString   T.Text
          | SBool     Bool
          | SInt      Int
          | SList     [Expr]
          | SSymbol   Symbol
          | SFunction Symbol
          | SBind     Symbol
          | SNil
          deriving (Eq)

instance Show Expr where
    show (SString b) = T.unpack b
    show (SBool   b) = show b
    show (SInt    i) = show i
    show (SSymbol s) = T.unpack s
    show (SList   l) = "'(" ++ unwords (map show l) ++ ")"
    show (SNil     ) = "Nil"

showType :: Expr -> T.Text
showType (SString _) = "String"
showType (SBool   _) = "Bool"
showType (SInt    _) = "Int"
showType (SSymbol _) = "Symbol"
showType (SList   _) = "List"
showType (SNil     ) = "Nil"

type Call st = [Expr] -> Scheme st Expr

data Scope st = Scope
  { symbols :: [(Symbol, Expr)]
  , functs  :: [(Symbol, Call st)] }

data Env st = Env
  { global      :: Scope st
  , context     :: [Scope st]
  , evalTimeout :: Int }


emptyScope :: Scope st
emptyScope = Scope [] []

emptyEnv :: Env st
emptyEnv = Env emptyScope [] 40000

class WithScheme st where
    getEnv :: Action st (Env st)
    putEnv :: Env st -> Action st ()

type Scheme st = ExceptT T.Text (Action st)

(...) :: T.Text -> T.Text -> T.Text
(...) = T.append

