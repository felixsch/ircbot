{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Network.IRC
  ( IrcError(..)
  , IrcServerSettings(..)
  , IrcConfig(..)
  , IrcState(..)
  , IrcStateRef
  , IrcT(..)
  , runIrcT
  , askRef
  , Name
  , Hostname
  , Channel
  , Cmd
  , Param
  , runIRC
  , createIrc
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.STM
import Control.Monad.Catch

import Control.Concurrent.STM.TVar

import Data.Monoid
import Network hiding (sClose, connectTo)
import Network.BSD
import Network.Socket hiding (send)
import Data.Char (chr)
import Data.ByteString.Char8



import qualified Data.Text as T

type Hostname = ByteString
type Name     = ByteString
type Channel  = ByteString
type Cmd      = ByteString
type Param    = ByteString


data IrcError = IrcError String
    deriving (Show)

instance Error IrcError where
    noMsg = IrcError "Unkown Error occured"
    strMsg = IrcError


data IrcServerSettings = IrcServerSettings
  { host     :: String
  , port     :: Int
  , channels :: [ByteString]
  , nick     :: ByteString
  , altNick  :: ByteString
  , realName :: ByteString
  }

data IrcConfig = IrcConfig
  { servers :: [IrcServerSettings]
  }

data IrcState = IrcState
  { connections :: [(ByteString, Socket)]
  }

type IrcStateRef = TVar IrcState

type IrcRuntime = (IrcConfig, IrcStateRef)


newtype IrcT m a = IrcT (ReaderT IrcRuntime (ErrorT IrcError m) a)
    deriving (Functor, Monad, MonadIO, MonadReader IrcRuntime)

    
instance (Functor m, Monad m) => Applicative (IrcT m) where
    pure = return
    (<*>) = ap

instance (Monad m, Monoid a) => Monoid (IrcT m a) where
    mempty = return mempty
    mappend = liftM2 mappend

askRef :: (Functor m, Monad m) => IrcT m IrcStateRef
askRef = snd <$> ask

askConfig :: (Functor m, Monad m) => IrcT m IrcConfig
askConfig = fst <$> ask


instance (Functor m, MonadIO m) => MonadState IrcState (IrcT m) where
    get = liftIO . readTVarIO =<< askRef
    put state = do
        ref <- askRef
        liftIO $ atomically $ writeTVar ref state


runIrcT :: (MonadIO m) => IrcConfig -> IrcStateRef -> IrcT m a -> m (Either IrcError a)
runIrcT config stateRef (IrcT a) = runErrorT $ runReaderT a (config,stateRef)


connectTo :: String -> PortID -> IO (String, Socket)
connectTo hostname (PortNumber po)= do
    proto <- getProtocolNumber "tcp"
    bracketOnError
        (socket AF_INET Stream proto)
        sClose
        (\s -> do
            h <- getHostByName hostname
            connect s (SockAddrInet po (hostAddress h))
            return (hostname, s)
        )
connectTo _ _ = error "failed..."


mkIrcState :: (MonadIO m) => m IrcStateRef
mkIrcState = liftIO $ atomically $ newTVar $ IrcState
  { connections = []
  }

runIRC :: (MonadIO m) => IrcConfig -> (IrcT m ()) -> m (Maybe IrcError)
runIRC config commands= do
    emptyState <- mkIrcState
    result <- runIrcT config emptyState commands

    return $ case result of
        Left x   -> Just x
        Right _  -> Nothing


createIrc :: (Functor m, MonadIO m) => IrcT m ()
createIrc = do
    config <- askConfig
    cons <- mapM connectToIrc $ servers config
    modify (\conf -> conf { connections = cons })
    
    
connectToIrc :: (MonadIO m) => IrcServerSettings -> IrcT m (ByteString, Socket)
connectToIrc server = liftIO $ do 
    proto <- getProtocolNumber "tcp"
    bracketOnError
        (socket AF_INET Stream proto)
        sClose
        (\s -> do
            h <- getHostByName $ host server
            connect s (SockAddrInet (fromIntegral $ port server) (hostAddress h))
            return (pack $ host server, s)
        )


