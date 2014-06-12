{-# LANGUAGE OverloadedStrings #-}


module Network.IRC.Conduit
  ( IRCConduit
  , ircGetLine
  , ircPutLine
  , runIRC
  , testCon
  ) where

import Control.Monad.IO.Class
import Control.Monad.Catch

import Network hiding (sClose, connectTo)
import Network.BSD
import Network.Socket
import Data.Char (chr)
import Data.ByteString (ByteString)
import Data.Text
import qualified Data.Text.IO as TIO
import Data.Text.Encoding
import Data.Conduit
import qualified Data.Conduit.List as CL
import Data.Conduit.Network


import Network.IRC.Message
import Network.IRC.Client

type IRCSource m  = Source m IRCMessage
type IRCSink m    = Sink IRCMessage m ()
type IRCConduit m = Conduit IRCMessage m IRCMessage


connectTo :: String -> PortID -> IO Socket
connectTo hostname (PortNumber port)= do
    proto <- getProtocolNumber "tcp"
    bracketOnError
        (socket AF_INET Stream proto)
        sClose
        (\s -> do
            h <- getHostByName hostname
            connect s (SockAddrInet port (hostAddress h))
            return s
        )




convertByteString :: (Monad m) => Conduit ByteString m Text
convertByteString = CL.map (decodeUtf8With utf8Failed)
    where
        utf8Failed _ (Just a) = Just $ chr $ fromEnum a
        utf8Failed _ Nothing  = Just '�'

convertText :: (Monad m) => Conduit Text m ByteString
convertText = CL.map encodeUtf8


ircGetLine :: (MonadIO m, MonadThrow m) => Conduit ByteString m IRCMessage
ircGetLine = convertByteString =$ decodeMessage


ircPutLine :: (Monad m) => Conduit IRCMessage m ByteString
ircPutLine = encodeMessage =$ convertText


runIRC :: (MonadIO m, MonadThrow m) => IRCClient -> IRCConduit m  -> m ()
runIRC client cmd = do
    sock <- liftIO $ connectTo (server client) (PortNumber $ fromIntegral $ port client)
    conduitIRC client cmd (sourceSocket sock $= ircGetLine) (ircPutLine =$ sinkSocket sock) 
    liftIO $ sClose sock
    

conduitIRC :: (MonadIO m) => IRCClient -> IRCConduit m -> IRCSource m -> IRCSink m -> m ()
conduitIRC client cmd src snk = setup
    where
        setup = src $$ cmd =$= setupIRC client =$ snk
    
setupIRC :: (MonadIO m) => IRCClient -> Conduit IRCMessage m IRCMessage
setupIRC client = do
    liftIO $ putStrLn "setupIRC"
    _ <- await
    yield $ raw "NICK" [nickname client]
    yield $ raw "USER" [nickname client, "0", "*", ':' `cons` realname client]
    mapM_ (\channel -> yield $ raw "JOIN" [channel]) $ channels client
    liftIO $ putStrLn "setupIRC -- done"

onAction :: (Monad m) => Text -> (IRCConduit m) -> IRCConduit m
onAction = undefined

onPrivMsg :: (Monad m) => (Nick -> Hostname -> IRCConduit m) -> IRCConduit m
onPrivMsg = undefined

onChan :: (Monad m) => (Channel -> IRCConduit m) -> IRCConduit m
onChan = undefined


testCon :: (MonadIO m) => Conduit IRCMessage m IRCMessage
testCon = CL.mapM $ \msg -> do
    liftIO . TIO.putStrLn $ '>' `cons` showMessage msg
    return msg







