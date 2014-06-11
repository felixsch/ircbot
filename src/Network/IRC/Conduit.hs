module Network.IRC.Conduit
  ( IRCSource
  , IRCConduit
  , IRCSink
  ) where

import Control.Monad.IO.Class

import Data.Conduit
import Data.Conduit.Network

import Network.IRC.Message
import Network.IRC.Client


type IRCSource m = Source m IRCMessage
type IRCConduit m a = Conduit IRCMessage m a 
type IRCSink m = Sink IRCMessage m ()




ircSource :: (MonadIO m) => Socket -> IRCSource m
ircSource s = sourceSocket s $= parseMessage










