{-# LANGUAGE OverloadedStrings #-}

module Scheme.Builtin 
 ( eval
 , builtin
 , newEnvWith
 ) where

import Control.Applicative
import Control.Monad.Except

import qualified Data.ByteString.Char8 as B

import Scheme.Types
import Scheme.Env

eval :: (WithScheme st) => Expr -> Scheme st Expr
eval (SSymbol s)    = findSymbol s
eval (SList (x:xs)) = push emptyScope *> (evalList xs =<< eval x) <* pop
eval x              = return x

evalList :: (WithScheme st) => [Expr] -> Expr -> Scheme st Expr
evalList a (SFunction f) = do
    func <- findFunc f
    args <- mapM eval a
    func args
evalList a (SBind f) = do
    func <- findFunc f
    func a
evalList _ _         = throwError "Not a expression"

newEnvWith :: (WithScheme st) => Int -> Env st
newEnvWith to = Env 
  { global      = builtin
  , context     = [] 
  , evalTimeout = to }

builtin :: (WithScheme st) => Scope st
builtin = Scope
  { symbols = builtinSymbols
  , functs  = builtinFuncts }

builtinSymbols :: [(Symbol, Expr)]
builtinSymbols = [ ("*", SFunction "mul")
                , ("+", SFunction "add")
                , ("-", SFunction "neg")
                , ("mul", SFunction "mul")
                , ("add", SFunction "add")
                , ("neg", SFunction "neg")
                , ("list", SFunction "list")
                , ("defun", SBind "defun")
                , ("quote", SFunction "quote")
                , ("lambda", SBind "lambda")
                , ("let", SBind "let")]

builtinFuncts :: (WithScheme st) => [(Symbol, Call st)]
builtinFuncts = [ ("mul", schemeMathOp "mul" (*))
                , ("add", schemeMathOp "add" (+))
                , ("neg", schemeMathOp "neg" (-))
                , ("list", return . SList)
                , ("defun", schemeDefun)
                , ("quote", schemeQuote)
                , ("lambda", schemeLambda)
                , ("let", schemeLet)]



invalidArguments :: B.ByteString -> Int -> Int -> Scheme st a
invalidArguments name should has = throwError $ 
    name ... " is applied to " ... has' ... " arguments, but it takes " ... should' ... " arguments"
    where
     should' = toNumName should
     has'    = toNumName has
     toNumName (-2) = "more than two"
     toNumName (-1) = "more than one"
     toNumName 0 = "zero"
     toNumName 1 = "one"
     toNumName 2 = "two"
     toNumName 3 = "three"
     toNumName 4 = "four"
     toNumName x = B.pack $ show x

invalidTypes :: B.ByteString -> B.ByteString -> [Expr] -> Scheme st a
invalidTypes name should exprs = throwError $
    name ... ": Coult not match " ... should ... " with " ... signature
    where
        signature = B.intercalate " -> " $ map showType exprs

toSymbol :: Expr -> Scheme st Symbol
toSymbol (SSymbol x) = return x
toSymbol x           = throwError $ "Symbol required but " ... showType x ... " found"

schemeQuote :: [Expr] -> Scheme st Expr
schemeQuote (x:[]) = return x
schemeQuote x      = invalidArguments "quote" 1 (length x)

schemeMathOp :: B.ByteString -> (Int -> Int -> Int) -> [Expr] -> Scheme st Expr
schemeMathOp name _  (_:[])   = invalidArguments name (-2) 1
schemeMathOp name _  []       = invalidArguments name (-2) 0
schemeMathOp name op e@(x:xs) = do
    when (typeTestFailed e) $ invalidTypes name "Int -> Int -> .." e
    foldM calc x xs
    where
        typeTestFailed (SInt _ : rest) = typeTestFailed rest
        typeTestFailed _             = False

        calc (SInt i) (SInt j) = return $ SInt $ i `op` j


functionSkel :: (WithScheme st) => Symbol -> [Symbol] -> Expr -> [Expr] -> Scheme st Expr
functionSkel symbol params body args = do
            when (length params /= length args) $ invalidArguments symbol (length params) (length args)
            push $ genScope $ zip params args 
            result <- eval body
            pop
            return result

schemeDefun :: (WithScheme st) => [Expr] -> Scheme st Expr
schemeDefun (SSymbol s : SList params : body : []) = do
    paramSymbols <- mapM toSymbol params

    withGlobal (return . newFunction s (functionSkel s paramSymbols body))

    return $ SFunction s
schemeDefun x   = invalidArguments "defun" 3 (length x)


schemeLambda :: (WithScheme st) => [Expr] -> Scheme st Expr
schemeLambda (SList params : body : []) = do
    env <- lift getEnv
    paramSymbols <- mapM toSymbol params

    withNScope 1 (return . newFunction (genName env) (functionSkel (genName env) paramSymbols body))

    return $ SFunction $ genName env

    where
      genName env = "lambda_" `B.append` B.pack (show $ num $ current env)
      num scope = length (functs scope)
schemeLambda x   = invalidArguments "lambda" 2 (length x)


schemeLet :: (WithScheme st) => [Expr] -> Scheme st Expr
schemeLet (SList defs : body : []) = do
    newSymbols <- mapM toPair defs

    push $ genScope newSymbols
    result <- eval body
    pop
    
    return result

    where 
        toPair (SList (SSymbol s : var : [])) = return (s,var)
        toPair (SList x)                      = invalidArguments "let assignment" 2 (length x)
schemeLet x   = invalidArguments "let" 2 (length x)




    







