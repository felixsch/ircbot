{-# LANGUAGE OverloadedStrings #-}

module Module.Scheme
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
import qualified Data.Text as T

import Module.Scheme.Parser
import Module.Scheme.Types
import Module.Scheme.Env
import Module.Scheme.Builtin

evalScheme :: (WithScheme st) => Expr -> Action st (Either T.Text Expr)
evalScheme expr = runExceptT (eval expr)
    

schemeEval :: (WithScheme st) => T.Text -> Action st ()
schemeEval tr = whenTrigger tr $ \dest params -> do
    env <- getEnv

    case parseExpr (T.unwords params) of
      Nothing   -> say dest "Syntax error"
      Just expr -> timeout (evalTimeout env) $ do
        result <- evalScheme expr
        say dest $ case result of
          Left err            -> err
          Right (SFunction s) -> s `T.append` " defined."
          Right (SBind s)     -> s `T.append` " defined."
          Right (SSymbol s)   -> s `T.append` " set."
          Right final         -> T.pack $ show final

schemeGetDefined :: (WithScheme st) => T.Text -> Action st ()
schemeGetDefined tr = whenTrigger tr $ \dest _ -> do
    env <- getEnv
    say dest $ "Function defined: " `T.append` (T.intercalate ", " $ defined (global env)) 
    where
     defined = map fst . functs


            
        
schemeClearState :: (WithScheme st) => T.Text -> Env st -> Action st ()
schemeClearState tr env = whenTrigger tr $ \dest _ -> 
    putEnv env >> say dest "Context cleared"
