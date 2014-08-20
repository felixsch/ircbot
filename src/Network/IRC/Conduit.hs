{-# LANGUAGE OverloadedStrings #-}


module Network.IRC.Conduit
  ( IrcData
  , IrcConduit
  --, IRCSource
  --, IRCSink
  , ircGet
  , ircPut
  --, runIRC
  --, send
  --, onAction
  --, onChannel
  ) where

import Control.Monad.IO.Class
import Control.Monad.Catch

import Network hiding (sClose, connectTo)
import Network.BSD
import Network.Socket hiding (send)
import Data.Char (chr)
import Data.ByteString.Char8
import Data.Conduit
import qualified Data.Conduit.List as CL
import Data.Conduit.Network

--import Network.IRC
import Network.IRC.Message
import Network.IRC.Command

type IrcData = Either Message Command

type IrcConduit m = Conduit IrcData m IrcData

ircGet :: (MonadIO m) => Conduit ByteString m IrcData
ircGet = ircParseMessage =$= toEither
  where
    toEither = awaitForever (yield . Left)


ircPut :: (MonadIO m) => Conduit IrcData m ByteString
ircPut = awaitForever fromEither =$= prepareCommand
    where
      fromEither (Left _) = return ()
      fromEither (Right c) = yield c

      prepareCommand = CL.map $ \x -> showCommand x `append` "\r\n"

ircParseMessage :: (MonadIO m) => Conduit ByteString m Message
ircParseMessage = awaitForever $ \t -> 
    case parseMessage t of
        Left _ -> return ()
        Right msg -> yield msg

    

{-connectIRC :: (MonadIO m) => IRCClient -> IRCConduit m -> Conduit IRC m IRC
connectIRC client worker = waitForNotice (4:: Int)
    where
      waitForNotice 0 = initIRC >> worker
      waitForNotice x = await >>= isValue >> waitForNotice (x -1) 

      isValue (Just x) = yield x
      isValue Nothing  = return ()

      initIRC = do
        liftIO $ putStrLn "initializing irc..."
        liftIO $ putStrLn "setting up nick..."
        send $ raw "NICK" [nickname client] Nothing
        liftIO $ putStrLn "setting up user..."
        send $ raw "USER" [nickname client, "0", "*", ':' `cons` realname client] Nothing 
        liftIO $ putStrLn "joining channels..."
        mapM_ (send . join) $ channels client -}
{-
send :: (Monad m) => Command -> ConduitM i IrcData m ()
send = yield . Right
-}


{-
runIRC :: (MonadIO m) => IRCClient -> IRCConduit m  -> m ()
runIRC client worker = do
    sock <- liftIO $ connectTo (server client) (PortNumber $ fromIntegral $ port client)
    sourceSocket sock $= ircGet =$= connectIRC client worker =$= ircPut $$ sinkSocket sock
    liftIO $ sClose sock -}
    

{-
onAction :: (MonadIO m) => Cmd -> IRCConduit m -> IRCConduit m
onAction action cb = awaitForever checkMessage
    where
        checkMessage msg@(Left m) = checkIf (cmd m) msg
        checkMessage x            = yield x

        checkIf a msg
          | a == action = leftover msg >> cb 
          | otherwise   = yield msg



onChannel :: (MonadIO m) => Channel -> IRCConduit m -> IRCConduit m
onChannel chan cb = onAction "PRIVMSG" $ awaitForever checkMessage
    where
      checkMessage msg@(Left m) = checkIfChannel (params m) msg
      checkMessage x            = yield x

      checkIfChannel (x:_) msg
       | chan == x  = leftover msg >> cb
       | otherwise  = yield msg -}
