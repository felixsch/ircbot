{-# LANGUAGE OverloadedStrings #-}

module Scheme.Env
  ( current
  , removeLastScope
  , consNewScope
  , genScope
  , newFunction
  , newSymbol
  , pop, pop'
  , push
  , withEnv
  , withGlobal
  , withScope
  , withNScope
  , findSymbol
  , findFunc
  ) where


import Control.Monad.State
import Control.Monad.Except

import qualified Data.ByteString.Char8 as B

import Scheme.Types


safeHead :: [Scope st] -> Scope st
safeHead []     = emptyScope
safeHead (x:_) = x

safeIndex :: Int -> [Scope st] -> Scope st
safeIndex i scopes
 | length scopes >= i = scopes !! i
 | otherwise          = emptyScope

current :: Env st -> Scope st
current = safeHead . context

removeLastScope :: Env st -> Env st
removeLastScope env = env { context = drop 1 (context env) }

consNewScope :: Scope st -> Env st -> Env st
consNewScope scope env = env { context =  scope : context env }

replaceScope :: Int -> Scope st -> Env st -> Env st
replaceScope i scope env = env { context = replace i scope (context env) }
    where
        replace n item ls = a ++ (item:b) where (a, _:b) = splitAt n ls

genScope :: [(Symbol, Expr)] -> Scope st
genScope = foldl gen emptyScope
    where
        gen scope (s, e) = newSymbol s e scope

newSymbol :: Symbol -> Expr -> Scope st -> Scope st
newSymbol symbol expr scope = scope { symbols = (symbol, expr) : symbols scope }

newFunction :: Symbol -> Call st -> Scope st -> Scope st
newFunction symbol call scope = scope { symbols = (symbol, SFunction symbol) : symbols scope
                                      , functs  = (symbol, call) : functs scope }

getAllSymbols :: [Scope st] -> [(Symbol, Expr)]
getAllSymbols = foldr ((++) . symbols) []

getAllFuncts :: [Scope st] -> [(Symbol, Call st)]
getAllFuncts = foldr ((++) . functs) []


push :: (WithScheme st) => Scope st -> Scheme st ()
push scope = withEnv (return . consNewScope scope) 

pop :: (WithScheme st) => Scheme st ()
pop = withEnv (return . removeLastScope)

pop' :: (WithScheme st) => Scheme st (Scope st)
pop' = do
    env <- lift getEnv
    lift $ putEnv $ removeLastScope env
    return $ current env

    

withEnv :: (WithScheme st) => (Env st -> Scheme st (Env st)) -> Scheme st ()
withEnv f = lift . putEnv =<< f =<< lift getEnv

withScope :: (WithScheme st) => (Scope st -> Scheme st (Scope st)) -> Scheme st ()
withScope f = withEnv $ \env -> do
    new <- f $ current env
    return $ consNewScope new $ removeLastScope env

withNScope :: (WithScheme st) => Int -> (Scope st -> Scheme st (Scope st)) -> Scheme st ()
withNScope n f = withEnv $ \env -> do
    new <- f $ safeIndex n (context env)
    return $ replaceScope n new env
    

withGlobal :: (WithScheme st) => (Scope st -> Scheme st (Scope st)) -> Scheme st ()
withGlobal f = withEnv $ \env -> do
    new <- f $ global env
    return $ env { global = new }


findSymbol :: (WithScheme st) => Symbol -> Scheme st Expr
findSymbol symbol = do
    env <- lift getEnv
    case lookup symbol (table env) of
        Nothing -> throwError $ "Could not find symbol " `B.append` symbol
        Just x  -> return x
    where
      table env = getAllSymbols (context env ++ [global env])

findFunc :: (WithScheme st) => Symbol -> Scheme st (Call st)
findFunc symbol = do
    ctx <- lift getEnv
    case lookup symbol (table ctx) of
        Nothing -> throwError $ "Could not find function " `B.append` symbol
        Just x  -> return x
    where
      table env = getAllFuncts (context env ++ [global env])
    
    
