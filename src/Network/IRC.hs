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
  , askRef
  , Name
  , Hostname
  , Channel
  , Cmd
  , Param
  , runIRC
  ) where

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

runIRC :: IrcConfig -> IO (Maybe IrcError)
runIRC config = do
    emptyState <- mkIrcState
    result <- runIrc' config emptyState setup
    forever $ liftIO $ threadDelay 100000
    return $ case result of
        Left x   -> Just x
        Right _  -> Nothing


setup :: Irc ()
setup = do
    config <- askConfig
    cons <- mapM connectToIrc $ servers config
    modify (\conf -> conf { connections = cons })

    forM_ cons $ \(server,(hdl,_)) -> void . fork . forever $ do
        line <- liftIO $ hGetLine hdl
        case parseMessage line of
            Just msg -> void $ fork (handleMessage server msg)
            Nothing  -> do
                ircLog Nothing $ "Mailformed message: " ++ (show $ unpack line)
                throwError $ MailformedMessage (show $ unpack $ line)

handleMessage :: Server -> Message -> Irc ()
handleMessage server msg = ircLog (Just $ unpack server) (unpack $ showMessage msg) 

send :: Command -> Irc ()
send cmd = do
    cons <- connections <$> get
    case lookup (commandDestination cmd) cons of
        Just (_,send) -> send $ showCommand cmd
        Nothing   -> throwError (UnknownServer $ commandDestination cmd)


signOn :: Irc ()
signOn = undefined
    

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

