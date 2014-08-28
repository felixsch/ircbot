{-# LANGUAGE OverloadedStrings #-}

module Scheme
  ( Env (..)
  , emptyEnv
  , newEnvWith
  , WithScheme(..)
  , schemeEval
  , schemeClearState
  , schemeGetDefined ) where



import Control.Monad.State
import Control.Monad.Except
import Control.Applicative

import Network.IRC
import Network.IRC.Action

import Data.List
import qualified Data.ByteString.Char8 as B

import Scheme.Parser
import Scheme.Types
import Scheme.Env
import Scheme.Builtin

evalScheme :: (WithScheme st) => Expr -> Action st (Either B.ByteString Expr)
evalScheme expr = runExceptT (eval expr)
    

schemeEval :: (WithScheme st) => B.ByteString -> Action st ()
schemeEval tr = whenTrigger tr $ \dest params -> do
    env <- getEnv

    case parseExpr (B.unwords params) of
      Nothing   -> say dest "Syntax error"
      Just expr -> timeout (evalTimeout env) $ do
        result <- evalScheme expr
        say dest $ case result of
          Left err            -> err
          Right (SFunction s) -> s `B.append` " defined."
          Right (SBind s)     -> s `B.append` " defined."
          Right (SSymbol s)   -> s `B.append` " set."
          Right final         -> B.pack $ show final

schemeGetDefined :: (WithScheme st) => B.ByteString -> Action st ()
schemeGetDefined tr = whenTrigger tr $ \dest _ -> do
    env <- getEnv
    say dest $ "Function defined: " `B.append` (B.intercalate ", " $ defined (global env)) 
    where
     defined = map fst . functs


            
        
schemeClearState :: (WithScheme st) => B.ByteString -> Env st -> Action st ()
schemeClearState tr env = whenTrigger tr $ \dest _ -> 
    putEnv env >> say dest "Context cleared"
