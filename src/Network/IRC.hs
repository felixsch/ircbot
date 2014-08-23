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
  , Irc(..)
  , Action(..)
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
  { connections :: [(Server, (Handle, (ByteString -> Irc ())))]
  , logHdl      :: Maybe Handle
  }

type IrcStateRef = TVar IrcState

type IrcRuntime = (IrcConfig, IrcStateRef)


newtype Irc a = Irc { runIrc :: ReaderT IrcRuntime (ErrorT IrcError IO) a }
    deriving (Functor, Monad, MonadIO, MonadReader IrcRuntime, MonadError IrcError)

instance MonadBase IO Irc where
    liftBase = Irc . liftBase

instance MonadBaseControl IO Irc where
    newtype StM Irc a = StIrc { unStIrc :: StM (ReaderT IrcRuntime (ErrorT IrcError IO)) a}

    liftBaseWith f = Irc (liftBaseWith (\runInIO -> f (fmap StIrc . runInIO . runIrc)))
    restoreM = Irc . restoreM . unStIrc
  

fork :: MonadBaseControl IO Irc => Irc () -> Irc ThreadId
fork = liftBaseDiscard forkIO

instance Applicative Irc where
    pure = return
    (<*>) = ap

instance (Monoid a) => Monoid (Irc a) where
    mempty = return mempty
    mappend = liftM2 mappend


askRef :: Irc IrcStateRef
askRef = snd <$> ask

askConfig :: Irc IrcConfig
askConfig = fst <$> ask


ircLog :: Maybe String -> String -> Irc ()
ircLog ext msg = do
    isVerbose <- verbose <$> askConfig
    hdl <- logHdl <$> get
    date <- formatTime defaultTimeLocale rfc822DateFormat <$> liftIO getCurrentTime

    when (isJust hdl) $ liftIO $ hPutStrLn (fromJust hdl) $ format date ext msg
    when isVerbose $ liftIO $ putStrLn $ format date ext msg
    where
        format date (Just mo) msg = "[" ++ date ++ "][" ++ mo ++ "] " ++ msg
        format date Nothing   msg = "[" ++ date ++ "] " ++ msg
    


instance MonadState IrcState Irc where
    get = liftIO . readTVarIO =<< askRef
    put state = do
        ref <- askRef
        liftIO $ atomically $ writeTVar ref state


runIrc' :: IrcConfig -> IrcStateRef -> Irc a -> IO (Either IrcError a)
runIrc' config stateRef (Irc a) = runErrorT $ runReaderT a (config,stateRef)


mkIrcState :: IO IrcStateRef
mkIrcState = do
    atomically $ newTVar $ IrcState
        { connections = []
        , logHdl      = Nothing
        }

runIRC :: IrcConfig -> Action () -> IO (Maybe IrcError)
runIRC config actions = do
    emptyState <- mkIrcState
    result <- runIrc' config emptyState (run actions)
    forever $ liftIO $ threadDelay 100000
    return $ case result of
        Left x   -> Just x
        Right _  -> Nothing


type Action a = StateT (Server,Message) Irc a

runAction :: Server -> Message -> Action () -> Irc ()
runAction server message action = void $ runStateT action (server, message)

onChannel :: Channel -> ([Param] -> Action ()) -> Action ()
onChannel channel cmd = checkIfChannel =<< get
  where
    checkIfChannel (server, (ChannelMsg chan _ params)) 
        | channel == chan = cmd params
        | otherwise = return ()
    checkIfChannel (server, msg@(Message _ "PRIVMSG" (x:xs)))
        | channel == x = cmd xs
        | otherwise    = return ()
    checkIfChannel _  = return ()

onPrivMsg :: (ByteString -> [Param] -> Action ()) -> Action ()
onPrivMsg action = do
    msg <- toGenericMessage . snd <$> get
    when (isCommand "PRIVMSG" msg) $ setupAction msg
    where
        setupAction msg@(Message _ _ (x:xs)) = action x xs
        

isCommand :: Cmd -> Message -> Bool
isCommand cmd (Message _ cmd' _)
    | cmd == cmd' = True
    | otherwise   = False

say :: Channel -> ByteString -> Action ()
say channel msg = do
    server <- fst <$> get
    lift $ send $ putChannel server channel msg




run :: Action () -> Irc ()
run actions = do
    config <- askConfig
    cons <- mapM connectToIrc $ servers config
    modify (\conf -> conf { connections = cons })

    forM_ cons $ \(server,_) -> void . fork $ signOn server

    forM_ cons $ \(server,(hdl,_)) -> void . fork . forever $ do
        line <- liftIO $ hGetLine hdl
        case parseMessage line of
            Just msg -> void $ fork (handleMessage server msg actions)
            Nothing  -> ircLog Nothing $ "Warning: Malformed message: " ++ (show $ unpack line)

sendPong :: Server -> Message -> Irc ()
sendPong server (PingMsg msg) = send $ pong server msg
sendPong _ _          = return ()

handleMessage :: Server -> Message -> Action () -> Irc ()
handleMessage server msg actions = do
    ircLog (Just $ unpack server) (unpack $ showMessage msg)
    when (isPingMsg msg) $ sendPong server msg
    runAction server msg actions

send :: Command -> Irc ()
send cmd = do
    cons <- connections <$> get
    case lookup server cons of
        Just (_,send) -> do
            ircLog (Just $ unpack server) $ "> " ++ show cmd
            send $ showCommand cmd `append` "\r\n"
        Nothing   -> throwError (UnknownServer $ commandDestination cmd)
    where
        server = commandDestination cmd


signOn :: Server -> Irc ()
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


    

sendIrc :: Handle -> ByteString -> Irc ()
sendIrc hdl msg = liftIO $ hPut hdl msg

connectToIrc :: IrcServerSettings -> Irc (ByteString, (Handle, (ByteString -> Irc())))
connectToIrc server = do
    ircLog Nothing $ "Connecting to " ++ host server
    liftIO $ do 
        hdl <- connectTo (host server) (PortNumber $ fromIntegral $ port server)
        return (pack $ host server, (hdl, sendIrc hdl))

--onChannel :: (Channel -> Action st m ()) -> Action st m ()
--onMessage :: (Nickname -> Cloak -> Action st m ()) -> Action st m ()

