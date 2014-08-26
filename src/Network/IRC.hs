{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}

module Network.IRC
  ( IrcError(..)
  , IrcServer(..)
  , IrcSettings(..)
  , Irc(..)
  , fork
  , timeout
  , runIrcMonad
  , Action(..)
  , irc
  , start
  , cleanup
  , send
  , logMessage
  , defaultIrcLogger
  , current
  , currentUser
  , runAction
  ) where

import Prelude hiding (unwords)


import Control.Applicative
import Control.Monad.Base
import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad.State
import Control.Monad.STM           ( atomically )
import Control.Concurrent          ( ThreadId, forkIO, threadDelay )
import Control.Concurrent.STM.TVar
import Control.Monad.Trans.Control

import Network
import Network.IRC.Message
import Network.IRC.Command

import Data.Monoid
import Data.Maybe       ( isJust, fromJust )
import Data.Time.Clock  ( getCurrentTime )
import Data.Time.Format ( formatTime )

import System.IO     ( hPutStrLn, hClose, Handle )
import System.Locale ( defaultTimeLocale, rfc822DateFormat )

import qualified System.Timeout as T  ( timeout )

import qualified Data.ByteString.Char8 as B



data IrcError = IrcError String
              | UnknownServer Server
              | HandleError String
              deriving (Show)

type IrcLogger st = Maybe B.ByteString -> B.ByteString -> Irc st ()

data IrcSettings st = IrcSettings 
  { servers           :: [(Server, (Maybe Handle, IrcServer))]
  , logger            :: IrcLogger st
  , actionTimeout     :: Int
  , userState         :: TVar st }


data IrcServer = IrcServer
  { host     :: B.ByteString
  , port     :: Int
  , channels :: [B.ByteString]
  , nick     :: B.ByteString
  , altNick  :: B.ByteString
  , realName :: B.ByteString
  , password :: Maybe B.ByteString }

type IrcRuntime st = TVar (IrcSettings st)

newtype Irc st a = Irc { runIrc :: ReaderT (IrcRuntime st) (ExceptT IrcError IO) a }
    deriving (Functor, Monad, MonadIO, MonadReader (IrcRuntime st), MonadError IrcError)

instance Applicative (Irc st) where
    pure = return
    (<*>) = ap

instance MonadBase IO (Irc st) where
    liftBase = Irc . lift . lift

instance MonadBaseControl IO (Irc st) where
    newtype StM (Irc st) a = StIrc { unStIrc :: StM (ReaderT (IrcRuntime st) (ExceptT IrcError IO)) a}

    liftBaseWith f = Irc (liftBaseWith (\runInM -> f (fmap StIrc . runInM . runIrc)))
    restoreM = Irc . restoreM . unStIrc

fork :: Irc st () -> Irc st ThreadId
fork = liftBaseDiscard forkIO

timeout :: Int -> Irc st () -> Irc st ()
timeout t m = void $ liftBaseWith (\runInIO -> T.timeout t (runInIO m)) 

instance (Monoid a) => Monoid (Irc st a) where
    mempty = return mempty
    mappend = liftM2 mappend

instance MonadState (IrcSettings st) (Irc st) where
    get = liftIO . readTVarIO =<< ask
    put sta = do
        ref <- ask
        liftIO $ atomically $ writeTVar ref sta

defaultIrcLogger :: Bool -> Maybe Handle -> IrcLogger st
defaultIrcLogger verbose hdl ext msg = do
    date <- formatTime defaultTimeLocale rfc822DateFormat <$> liftIO getCurrentTime

    when (isJust hdl) $ liftIO $ hPutStrLn (fromJust hdl) $ format date ext (B.unpack msg)

    when verbose $ liftIO $ putStrLn $ format date ext (B.unpack msg)
    where
        format date (Just mo) m = "[" ++ date ++ "][" ++ B.unpack mo ++ "] " ++ m
        format date Nothing   m = "[" ++ date ++ "] " ++ m



mkRuntime :: [IrcServer] -> st -> IrcLogger st -> IO (IrcRuntime st)
mkRuntime srvs ste lo = do
    stref <- atomically $ newTVar ste
    atomically $ newTVar IrcSettings
      { servers        = initConnections srvs
      , actionTimeout  = 20000000
      , logger         = lo
      , userState      = stref }
    where
      initConnections (x:xs) = (host x, (Nothing, x)) : initConnections xs
      initConnections _     = []

runIrcMonad :: [IrcServer] -> st -> IrcLogger st -> Irc st () -> IO (Maybe IrcError)
runIrcMonad srvs sta lo (Irc m) = do
    runtime <- mkRuntime srvs sta lo
    result  <- runExceptT $ runReaderT m runtime
    return $ case result of
        Left x  -> Just x
        Right _ -> Nothing

data ActionRuntime st = ActionRuntime
  { currentServer :: Server
  , currentMessage :: Message
  , userStateRef :: TVar st }

newtype Action st a = Action (ReaderT (ActionRuntime st) (Irc st) a)
    deriving (Functor, Monad, MonadIO, MonadReader (ActionRuntime st))

instance Applicative (Action st) where
    pure = return
    (<*>) = ap

instance MonadBase (Irc st) (Action st) where
    liftBase = Action . lift

instance MonadState st (Action st) where
    get = liftIO . readTVarIO =<< (userStateRef <$> ask)
    put sta = do
        ref <- userStateRef <$> ask
        liftIO $ atomically $ writeTVar ref sta

current :: Action st (Server, Message)
current = (,) <$> server <*> message
    where
        server  = currentServer <$> ask
        message = currentMessage <$> ask

currentUser :: Action st Name
currentUser = getName . msgOrigin . currentMessage <$> ask 
    where
        getName (Just (Host n))       = n
        getName (Just (Nickname n _)) = n
        getName _                     = B.empty


irc :: Irc st a -> Action st a
irc = liftBase

runAction :: Server -> Message -> Action st () -> Irc st ()
runAction server message (Action action) = do
    runtime <- get
    void $ fork $ timeout (actionTimeout runtime) $ runReaderT action (mkActionRuntime $ userState runtime)
    where
        mkActionRuntime = ActionRuntime server message

logMessage :: Maybe B.ByteString -> B.ByteString -> Irc st ()
logMessage ext msg = do
    l <- logger <$> get
    l ext msg

send :: Cmd -> Irc st ()
send cmd = do
    co <- servers <$> get
    case lookup server co of
        Just (Just hdl, _) -> do
            logMessage (Just server) $ "> " `B.append` B.pack (show cmd)
            liftIO $ B.hPut hdl $ showCmd cmd `B.append` "\r\n"
        Nothing   -> throwError (UnknownServer server)
    where
        server = cmdDestination cmd


start :: Action st () -> Irc st ()
start action = do
    settings <- get
    srvs <- mapM connectToIrc $ servers settings
    modify (\set -> set { servers = srvs })

    forM_ srvs $ \(server,_) -> void . fork $ signOn server

    forM_ srvs $ \(server,(hdl,_)) -> void . fork . forever $
        case hdl of
          Just h -> handle server h
          Nothing -> logMessage Nothing $ "Warning: Malformed server handle: " `B.append` server       
    where
      handle server hdl = do
        line <- liftIO $ B.hGetLine hdl
        case parseMessage line of
            Just msg -> run server msg action
            Nothing  -> logMessage Nothing $ "Warning: Malformed message: " `B.append` line
      run server msg action' = do
        logMessage (Just server) (showMessage msg)
        when (msgCommand msg == "PING") (send $ mkPong server (B.unwords $ msgParams msg))
        runAction server msg action'
   

cleanup :: Irc st ()
cleanup = do
    srvs <- servers <$> get
    forM_ srvs $ \(_, (maybeHdl, _)) -> case maybeHdl of
        Just hdl -> liftIO $ hClose hdl
        Nothing  -> return ()

connectToIrc :: (Server,(Maybe Handle, IrcServer)) -> Irc st (Server, (Maybe Handle, IrcServer))
connectToIrc (h, (Nothing, server)) = do
    logMessage Nothing $ "Connection to " `B.append` h
    liftIO $ do 
        hdl <- connectTo (B.unpack h) (PortNumber $ fromIntegral $ port server)
        return (h, (Just hdl, server))
connectToIrc settings@(h,_) = logMessage (Just h) "Host is already connected" >> return settings


signOn :: Server -> Irc st ()
signOn server = do
    config <- getServerConfig
    liftIO $ threadDelay 2000000
    logMessage (Just server) "Setup nickname..."
    send $ setNickname server (nick config)

    logMessage (Just server) "Setup alternative nickname / realname..."
    send $ setUsername server (altNick config) (realName config)
    liftIO $ threadDelay 2000000

    logMessage (Just server) "Joining channels..."
    mapM_ (send . mkJoin server) (channels config)
    where
        getServerConfig = do 
            conf <- findServer . servers <$> get
            case conf of
                Just x  -> return x
                Nothing -> throwError $ IrcError "Unvalid server settings. Server config not found"
 
        findServer ((h,(_, conf)):xs)
            | h == server = Just conf
            | otherwise                 = findServer xs
        findServer []     = Nothing
