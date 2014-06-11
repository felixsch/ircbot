module Network.IRC.Conduit
  ( IRCSource
  , IRCConduit
  , IRCSink
  ) where

import Control.Monad.IO.Class
import Control.Monad.Catch

import Data.Char (chr)
import Data.ByteString 
import Data.Text hiding (pack)
import Data.Text.Encoding
import Data.Conduit
import qualified Data.Conduit.List as CL
import Data.Conduit.Network


import Network.IRC.Message
import Network.IRC.Client


type IRCConduit m = Conduit IRCMessage m (Maybe IRCMessage)




convertByteString :: (Monad m) => Conduit ByteString m Text
convertByteString = CL.map (decodeUtf8With utf8Failed)
    where
        utf8Failed _ (Just a) = Just $ chr $ fromEnum a
        utf8Failed _ Nothing  = Just '�'

convertText :: (Monad m) => Conduit Text m ByteString
convertText = CL.map encodeUtf8


ircGetLine :: (MonadThrow m) => Consumer ByteString m IRCMessage
ircGetLine = convertByteString =$ decodeMessage

ircPutLine :: (Monad m) => Conduit IRCMessage m ByteString
ircPutLine = encodeMessage =$ convertText



runIRC :: (MonadIO m, MonadThrow m) => IRCClient -> IRCConduit m  -> m ()
runIRC client cmd = liftIO $ runTCPClient settings $ conduitIRC client cmd
    where
        settings = clientSettings (port client) (pack $ server client)



conduitIRC :: (Monad m, MonadThrow m) => IRCClient -> IRCConduit m -> AppData -> IO ()
conduitIRC = undefined


onAction :: (Monad m) => Text -> (IRCConduit m) -> IRCConduit m
onAction = undefined

onPrivMsg :: (Monad m) => (Nick -> Hostname -> IRCConduit m) -> IRCConduit m
onPrivMsg = undefined

onChan :: (Monad m) => (Channel -> IRCConduit m) -> IRCConduit m
onChan = undefined








