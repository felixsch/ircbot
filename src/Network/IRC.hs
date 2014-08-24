{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module Network.IRC
  ( IrcError(..)
  , IrcServerSettings(..)
  , IrcConfig(..)
  , IrcState(..)
  , IrcStateRef
  , IrcT(..)
  , MonadIrc(..)
  , Action(..)
  , liftAction
  , say
  , askRef
  , Name
  , Hostname
  , Channel
  , Cmd
  , Param
  , runIRC
  , onChannel
  , onPrivMsg
  , ircLog
  , whenTrigger
  ) where

import Prelude hiding (unwords)


import Control.Applicative
import Control.Monad
import Control.Monad.Base
import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.STM
import Control.Monad.Catch
import Control.Monad.Trans.Control

import Control.Concurrent
import Control.Concurrent.STM.TVar

import Data.Monoid
import Data.Maybe
import Data.Char (chr)
import Data.ByteString.Char8 hiding (putStrLn, hPutStrLn)
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
  , verbose :: Bool
  }

data IrcState = IrcState
  { connections :: [(Server, Handle)]
  , logHdl      :: Maybe Handle
  }

type IrcStateRef = TVar IrcState

type IrcRuntime = (IrcConfig, IrcStateRef)




newtype IrcT m a = IrcT { runIrcT :: ReaderT IrcRuntime (ErrorT IrcError m) a }
    deriving (Functor, Monad, MonadIO, MonadReader IrcRuntime, MonadError IrcError)

class (Functor m, Monad m, MonadIO m, MonadBaseControl IO m) => MonadIrc m 

instance (MonadIrc m) => MonadIrc (IrcT m)
instance MonadIrc IO

instance MonadTrans IrcT where
    lift = IrcT . lift . lift

instance (MonadIrc m) => MonadBase IO (IrcT m) where
    liftBase = IrcT . liftBase


instance (MonadIrc m) => MonadBaseControl IO (IrcT m) where
    newtype StM (IrcT m) a = StIrcT { unStIrc :: StM (ReaderT IrcRuntime (ErrorT IrcError m)) a}

    liftBaseWith f = IrcT (liftBaseWith (\runInM -> f (fmap StIrcT . runInM . runIrcT)))
    restoreM = IrcT . restoreM . unStIrc
  
fork :: MonadBaseControl IO (IrcT m) => IrcT m () -> IrcT m ThreadId
fork = liftBaseDiscard forkIO

instance (MonadIrc m) => Applicative (IrcT m) where
    pure = return
    (<*>) = ap

instance (Monoid a, MonadIrc m) => Monoid (IrcT m a) where
    mempty = return mempty
    mappend = liftM2 mappend


askRef :: (MonadIrc m) => IrcT m IrcStateRef
askRef = snd <$> ask

askConfig :: (MonadIrc m) => IrcT m IrcConfig
askConfig = fst <$> ask


ircLog :: (MonadIrc m) => Maybe String -> String -> IrcT m ()
ircLog ext msg = do
    isVerbose <- verbose <$> askConfig
    hdl <- logHdl <$> get
    date <- formatTime defaultTimeLocale rfc822DateFormat <$> liftIO getCurrentTime

    when (isJust hdl) $ liftIO $ hPutStrLn (fromJust hdl) $ format date ext msg
    when isVerbose $ liftIO $ putStrLn $ format date ext msg
    where
        format date (Just mo) msg = "[" ++ date ++ "][" ++ mo ++ "] " ++ msg
        format date Nothing   msg = "[" ++ date ++ "] " ++ msg
    


instance (MonadIrc m) => MonadState IrcState (IrcT m) where
    get = liftIO . readTVarIO =<< askRef
    put state = do
        ref <- askRef
        liftIO $ atomically $ writeTVar ref state


runIrc' :: IrcConfig -> IrcStateRef -> IrcT m a -> m (Either IrcError a)
runIrc' config stateRef (IrcT a) = runErrorT $ runReaderT a (config,stateRef)


mkIrcState :: (MonadIrc m) => m IrcStateRef
mkIrcState = liftIO $
    atomically $ newTVar IrcState
        { connections = []
        , logHdl      = Nothing
        }

runIRC :: (MonadIrc m) => IrcConfig -> Action m () -> m (Maybe IrcError)
runIRC config actions = do
    emptyState <- mkIrcState
    result <- runIrc' config emptyState (run actions)
    forever $ liftIO $ threadDelay 100000
    return $ case result of
        Left x   -> Just x
        Right _  -> Nothing

newtype Action m a = Action (ReaderT (Server,Message) (IrcT m) a)
    deriving (Functor, Monad, MonadIO, MonadReader (Server,Message))

instance MonadTrans Action where
    lift = Action . lift . lift

instance (MonadIrc m) => Applicative (Action m) where
    pure = return
    (<*>) = ap


instance (MonadIrc m) => MonadBase (IrcT m) (Action m) where
    liftBase = Action . lift


--type Action m a = StateT (Server,Message) (IrcT m) a

liftAction :: (MonadIrc m) => m a -> Action m a
liftAction = Action . lift . lift

runAction :: (MonadIrc m) => Server -> Message -> Action m () -> IrcT m ()
runAction server message (Action action) = void $ runReaderT action (server, message)

onChannel :: (MonadIrc m) => Channel -> ([Param] -> Action m ()) -> Action m ()
onChannel channel cmd = checkIfChannel =<< ask
    where
    checkIfChannel (server, msg@(Message _ "PRIVMSG" (x:xs)))
        | channel == x = cmd xs
        | otherwise    = return ()
    checkIfChannel _  = return ()

onPrivMsg :: (MonadIrc m) => (ByteString -> [Param] -> Action m ()) -> Action m ()
onPrivMsg action = do
    msg <- snd <$> ask
    when (isCommand "PRIVMSG" msg) $ setupAction msg
    where
        setupAction msg@(Message _ _ (x:xs)) = action x xs
    

whenTrigger :: (MonadIrc m) => ByteString -> (ByteString -> [Param] -> Action m ()) -> Action m ()
whenTrigger trigger cmd = onPrivMsg checkTrigger
    where
        checkTrigger dest (x:params)
          | x == trigger  = cmd dest params
          | otherwise     = return ()

isCommand :: Cmd -> Message -> Bool
isCommand cmd (Message _ cmd' _)
    | cmd == cmd' = True
    | otherwise   = False

lifts :: (MonadIrc m) => IrcT m a -> Action m a
lifts = liftBase

say :: (MonadIrc m) => Channel -> ByteString -> Action m ()
say channel msg = do
    server <- fst <$> ask
    liftBase $ send $ putChannel server channel msg




run :: (MonadIrc m) => Action m () -> IrcT m ()
run actions = do
    config <- askConfig
    cons <- mapM connectToIrc $ servers config
    modify (\conf -> conf { connections = cons })

    forM_ cons $ \(server,_) -> void . fork $ signOn server

    forM_ cons $ \(server,hdl) -> void . fork . forever $ do
        line <- liftIO $ hGetLine hdl
        case parseMessage line of
            Just msg -> void $ fork (handleMessage server msg actions)
            Nothing  -> ircLog Nothing $ "Warning: Malformed message: " ++ (show $ unpack line)

handleMessage :: (MonadIrc m ) => Server -> Message -> Action m () -> IrcT m ()
handleMessage server msg actions = do
    ircLog (Just $ unpack server) (unpack $ showMessage msg)
    when (msgCommand msg == "PING") (send $ pong server (unwords $ msgParams msg))
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

--onChannel :: (Channel -> Action st m ()) -> Action st m ()
--onMessage :: (Nickname -> Cloak -> Action st m ()) -> Action st m ()

