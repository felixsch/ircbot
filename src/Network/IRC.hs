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
  , current
  , runAction
  ) where

import Prelude hiding (unwords)


import Control.Applicative
import Control.Monad
import Control.Monad.Base
import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad.State
import Control.Monad.STM
import Control.Monad.Catch
import Control.Monad.Trans.Control
import Control.Monad.Trans.Except
import qualified System.Timeout as T ( timeout )
import Control.Concurrent
import Control.Concurrent.STM.TVar

import Data.Monoid
import Data.Maybe
import Data.Char (chr)
import Data.ByteString.Char8 hiding (putStrLn, hPutStrLn)
import qualified Data.ByteString.Char8 as B
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (formatTime)

import Network

import Network.IRC.Message
import Network.IRC.Command

import System.IO hiding (hGetLine)
import System.Locale (defaultTimeLocale, rfc822DateFormat)

data IrcError = IrcError String
              | UnknownServer Server
              | MailformedMessage String

instance Show IrcError where
    show (IrcError str) = "Error: Unknown error"
    show (UnknownServer srv) = "Trying to send to a unknown server: " ++ unpack srv
    show (MailformedMessage msg) = "Recieved mailformed message (message was " ++ msg ++ ")"

type IrcLogger st = ((Maybe B.ByteString) -> B.ByteString -> Irc st ())

data IrcSettings st = IrcSettings 
  { servers           :: [(Server, (Maybe Handle, IrcServer))]
  , logger            :: IrcLogger st
  , actionTimeout     :: Int
  , userState         :: TVar st }


data IrcServer = IrcServer
  { host     :: B.ByteString
  , port     :: Int
  , channels :: [ByteString]
  , nick     :: ByteString
  , altNick  :: ByteString
  , realName :: ByteString
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
    put state = do
        ref <- ask
        liftIO $ atomically $ writeTVar ref state

defaultIrcLog :: Bool -> Handle -> IrcLogger st
defaultIrcLog verbose hdl ext msg = do
    date <- formatTime defaultTimeLocale rfc822DateFormat <$> liftIO getCurrentTime

    liftIO $ hPutStrLn hdl $ format date ext (unpack msg)
    when verbose $ liftIO $ putStrLn $ format date ext (unpack msg)
    where
        format date (Just mo) msg = "[" ++ date ++ "][" ++ unpack mo ++ "] " ++ msg
        format date Nothing   msg = "[" ++ date ++ "] " ++ msg

mkRuntime :: [IrcServer] -> st -> IrcLogger st -> IO (IrcRuntime st)
mkRuntime srvs ste lo = do
    stref <- atomically $ newTVar ste
    atomically $ newTVar IrcSettings
      { servers        = initConnections srvs
      , actionTimeout  = 200000
      , logger         = lo
      , userState      = stref }
    where
      initConnections (x:xs) = (host x, (Nothing, x)) : initConnections xs
      initConnections _     = []

runIrcMonad :: [IrcServer] -> st -> IrcLogger st -> Irc st () -> IO (Maybe IrcError)
runIrcMonad servers state logger (Irc m) = do
    runtime <- mkRuntime servers state logger
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
    put state = do
        ref <- userStateRef <$> ask
        liftIO $ atomically $ writeTVar ref state

current :: Action st (Server, Message)
current = (,) <$> server <*> message
    where
        server  = currentServer <$> ask
        message = currentMessage <$> ask

irc :: Irc st a -> Action st a
irc = liftBase

runAction :: Server -> Message -> Action st () -> Irc st ()
runAction server message (Action action) = do
    runtime <- get
    timeout (actionTimeout runtime) $ void $ fork $ runReaderT action (mkActionRuntime $ userState runtime)
    where
        mkActionRuntime = ActionRuntime server message

logMessage :: (Maybe B.ByteString) -> B.ByteString -> Irc st ()
logMessage ext msg = do
    l <- logger <$> get
    l ext msg

send :: Cmd -> Irc st ()
send cmd = do
    cons <- servers <$> get
    case lookup server cons of
        Just (Just hdl, _) -> do
            logMessage (Just server) $ "> " `B.append` (pack $ show cmd)
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
          Nothing -> return ()       
    where
      handle server hdl = do
        line <- liftIO $ hGetLine hdl
        case parseMessage line of
            Just msg -> runAction server msg action
            Nothing  -> logMessage Nothing $ "Warning: Malformed message: " `append` line
   

cleanup :: Irc st ()
cleanup = do
    srvs <- servers <$> get
    forM_ srvs $ \(_, (maybeHdl, _)) -> case maybeHdl of
        Just hdl -> liftIO $ hClose hdl
        otherwise -> return ()

connectToIrc :: (Server,(Maybe Handle, IrcServer)) -> Irc st (Server, (Maybe Handle, IrcServer))
connectToIrc (h, (Nothing, server)) = do
    logMessage Nothing $ "Connection to " `append` h
    liftIO $ do 
        hdl <- connectTo (unpack h) (PortNumber $ fromIntegral $ port server)
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



{-

run :: (MonadIrc m) => Action m () -> IrcT m ()
run actions = do
    config <- askConfig
    cons <- mapM connectToIrc $ servers config
    modify (\conf -> conf { connections = cons })

    forM_ cons $ \(server,_) -> void . fork $ signOn server

    forM_ cons $ \(server,hdl) -> void . fork . forever $ do
        line <- liftIO $ hGetLine hdl
        case parseMessage line of
            Just msg -> handleMessage server msg actions
            Nothing  -> ircLog Nothing $ "Warning: Malformed message: " ++ (show $ unpack line)
    modify (\state -> state { test = "hurray" })

handleMessage :: (MonadIrc m ) => Server -> Message -> Action m () -> IrcT m ()
handleMessage server msg actions = do
    ircLog (Just $ unpack server) (unpack $ showMessage msg)
    when (msgCommand msg == "PING") (send $ pong server (unwords $ msgParams msg))
    dat
    runAction server msg actions

send :: (MonadIrc m) => Command -> IrcT m ()
send cmd = do
    cons <- connections <$> get
    case lookup server cons of
        Just hdl -> do
            ircLog (Just $ unpack server) $ "> " ++ show cmd
            sendIrc hdl $ showCommand cmd `append` "\r\n"
        Nothing   -> throwError (UnknownServer $ commandDestination cmd)
    where
        server = commandDestination cmd

sendIrc :: (MonadIrc m) => Handle -> ByteString -> IrcT m ()
sendIrc hdl msg = liftIO $ hPut hdl msg

signOn :: (MonadIrc m) => Server -> IrcT m ()
signOn server = do
    config <- getServerConfig
    liftIO $ threadDelay 2000000
    ircLog (Just $ unpack server) "Setup nickname..."
    send $ setNickname server (nick config)

    ircLog (Just $ unpack server) "Setup alternative nickname / realname..."
    send $ setUsername server (altNick config) (realName config)
    liftIO $ threadDelay 2000000

    ircLog (Just $ unpack server) "Joining channels..."
    mapM_ (send . joinChannel server) (channels config)
 

    where
        getServerConfig = do 
            conf <- findServer . servers <$> askConfig
            case conf of
                Just x  -> return x
                Nothing -> throwError $ IrcError "Unvalid server settings. Server config not found"
 
        findServer (x:xs)
            | (pack $ host x) == server = Just x
            | otherwise                 = findServer xs
        findServer []     = Nothing


    


connectToIrc :: (MonadIrc m) => IrcServerSettings -> IrcT m (ByteString, Handle)
connectToIrc server = do
    ircLog Nothing $ "Connecting to " ++ host server
    liftIO $ do 
        hdl <- connectTo (host server) (PortNumber $ fromIntegral $ port server)
        return (pack $ host server, hdl)
-}
--onChannel :: (Channel -> Action st m ()) -> Action st m ()
--onMessage :: (Nickname -> Cloak -> Action st m ()) -> Action st m ()

