{-# LANGUAGE OverloadedStrings #-}

module Network.IRC.Action
  ( say
  , me
  , notice
  , join
  , logM
  , onChannel
  , onPrivMsg
  , whenTrigger
  )
  where

import Control.Applicative
import Control.Monad        ( when )

import Network.IRC
import Network.IRC.Command
import Network.IRC.Message
import qualified Data.ByteString.Char8 as B

type Destination = B.ByteString

say :: B.ByteString -> B.ByteString -> Action st ()
say dest msg = do
    server <- fst <$> current
    irc $ send $ mkPut server dest msg

    
me :: Destination -> B.ByteString -> Action st ()
me dest msg = do
    server <- fst <$> current
    irc $ send $ mkMe server dest msg

notice :: Name -> B.ByteString -> Action st ()
notice user msg = do
    server <- fst <$> current
    irc $ send $ mkNotice server user msg

join :: Destination -> Action st ()
join channel = do
    server <- fst <$> current
    irc $ send $ mkJoin channel channel

type ActionName = B.ByteString

logM :: ActionName -> B.ByteString -> Action st ()
logM name msg = irc $ logMessage (Just name) msg


onChannel :: Channel -> ([Param] -> Action st ()) -> Action st ()
onChannel channel cmd = checkIfChannel =<< current
    where
        checkIfChannel (server, msg@(Message _ "PRIVMSG" (x:xs)))
          | channel == x = cmd xs
          | otherwise    = return ()
        checkIfChannel _  = return ()

onPrivMsg :: (B.ByteString -> [Param] -> Action st ()) -> Action st ()
onPrivMsg action = do
    msg <- snd <$> current
    when (isCommand "PRIVMSG" msg) $ setupAction msg
    where
        setupAction msg@(Message _ _ (x:xs)) = action x xs


whenTrigger :: B.ByteString -> (B.ByteString -> [Param] -> Action m ()) -> Action m ()
whenTrigger trigger cmd = onPrivMsg checkTrigger
    where
        checkTrigger dest (x:params)
          | x == trigger = cmd dest params
          | otherwise = return ()

isCommand :: B.ByteString -> Message -> Bool
isCommand cmd (Message _ cmd' _)
    | cmd == cmd' = True
    | otherwise   = False
