{-# LANGUAGE OverloadedStrings #-}

module Scheme
  ( SchemeCtx(..)
  , SchemeState(..)
  , WithScheme(..)
  , mkSchemeSt
  , initScheme
  , schemeEval
  , schemeClearState ) where



import Control.Monad.State
import Control.Monad.Except
import Control.Applicative

import Network.IRC
import Network.IRC.Action

import Data.List
import qualified Data.ByteString.Char8 as B

import SchemeExpr

(...) :: B.ByteString -> B.ByteString -> B.ByteString
(...) = B.append


type Call st = [Expr] -> Scheme st Expr

data SchemeCtx st = SchemeCtx
  { symbols :: [(Symbol, Expr)]
  , functs  :: [(Symbol, Call st)] }

data SchemeState st = SchemeState
  { global  :: SchemeCtx st
  , env     :: [SchemeCtx st]
  , evalTimeout :: Int }

mkSchemeSt :: (WithScheme st) => SchemeState st
mkSchemeSt = SchemeState 
  { global  = builtin
  , env     = []
  , evalTimeout = 40000 }


ctxAddSymbol :: SchemeCtx st -> Symbol -> Expr -> SchemeCtx st
ctxAddSymbol ctx symbol expr = ctx { symbols = (symbol,expr) : symbols ctx }

ctxAddFun :: SchemeCtx st -> Symbol -> Call st -> SchemeCtx st
ctxAddFun ctx symbol call = ctx { symbols = (symbol, SFunction symbol) : symbols ctx
                                , functs  = (symbol, call) : functs ctx }

stPushCtx :: SchemeState st -> SchemeCtx st -> SchemeState st
stPushCtx state ctx = state { env = ctx : (env state) }

stPopCtx :: SchemeState st -> SchemeState st
stPopCtx state = state { env = drop 1 (env state) }



class WithScheme st where
    getScheme :: Action st (SchemeState st)
    putScheme :: SchemeState st -> Action st ()


getAllSymbols :: [SchemeCtx st] -> [(Symbol, Expr)]
getAllSymbols = foldr ((++) . symbols) []

getAllFuncts :: [SchemeCtx st] -> [(Symbol, Call st)]
getAllFuncts = foldr ((++) . functs) []


type Scheme st = ExceptT B.ByteString (Action st)

findSymbol :: (WithScheme st) => Symbol -> Scheme st Expr
findSymbol symbol = do
    ctx <- lift getScheme
    case lookup symbol (table ctx) of
        Nothing -> throwError $ "Could not find symbol " ... symbol
        Just x  -> return x
    where
      table ctxs = getAllSymbols (global ctxs : env ctxs)

findFunc :: (WithScheme st) => Symbol -> Scheme st (Call st)
findFunc symbol = do
    ctx <- lift getScheme
    case lookup symbol (table ctx) of
        Nothing -> throwError $ "Could not find function " ... symbol
        Just x  -> return x
    where
      table ctxs = getAllFuncts (global ctxs : env ctxs)


eval :: (WithScheme st) => Expr -> Scheme st Expr
eval (SSymbol s)    = findSymbol s
eval (SList (x:xs)) = evalList xs =<< eval x
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
    


evalScheme :: (WithScheme st) => Expr -> Action st (Either B.ByteString Expr)
evalScheme expr = runExceptT (eval expr)
    

initScheme :: (WithScheme st) => Action st ()
initScheme = putScheme mkSchemeSt

schemeEval :: (WithScheme st) => B.ByteString -> Action st ()
schemeEval tr = whenTrigger tr $ \dest params -> do
    schm <- getScheme

    case parseExpr (B.unwords params) of
      Nothing   -> say dest "Syntax error"
      Just expr -> timeout (evalTimeout schm) $ do
        result <- evalScheme expr
        say dest $ case result of
          Left err    -> err
          Right final -> B.pack $ show final
            
        
schemeClearState :: (WithScheme st) => B.ByteString -> Action st ()
schemeClearState tr = whenTrigger tr $ \dest _ -> 
    initScheme >> say dest "Scheme context cleared"




builtin :: (WithScheme st) => SchemeCtx st
builtin = SchemeCtx 
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
                , ("quote", SFunction "quote") ]

builtinFuncts :: (WithScheme st) => [(Symbol, Call st)]
builtinFuncts = [ ("mul", schemeMathOp "mul" (*))
                , ("add", schemeMathOp "add" (+))
                , ("neg", schemeMathOp "neg" (-))
                , ("list", return . SList)
                , ("defun", schemeDefun)
                , ("quote", schemeQuote) ]



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
        typeTestFailed ((SInt _)    : xs) = typeTestFailed xs
        typeTestFailed _                  = False

        calc (SInt x) (SInt y) = return $ SInt $ x `op` y

schemeDefun :: (WithScheme st) => [Expr] -> Scheme st Expr
schemeDefun ((SSymbol s):(SList params):body:[]) = do
    ctx <- lift getScheme
    paramSymbols <- mapM toSymbol params

    lift $ putScheme $ ctx { global = ctxAddFun (global ctx) s (fun paramSymbols) }

    return $ SFunction s
    where
        gen ctx (s, e) = ctxAddSymbol ctx s e
        newCtx         = SchemeCtx [] []
        fun par args   = do
            when (length par /= length args) $ invalidArguments s (length par) (length args)
            schm <- lift $ getScheme
            let ctx = foldl gen newCtx $ zip par args
            lift $ putScheme $ stPushCtx schm ctx
            result <- eval body
            schm' <- lift $ getScheme
            lift $ putScheme $ stPopCtx schm'
            return result
schemeDefun x   = invalidArguments "defun" 3 (length x)





