{-# LANGUAGE OverloadedStrings #-}

module Network.IRC.Command
  ( Command(..)
  , Channel
  , Server
  , showCommand
  , commandDestination
  , raw
  , pong
  , joinChannel
  , setNickname
  , setUsername
  , putChannel
  ) where

import Prelude hiding (unwords)

import Data.ByteString.Char8
import Data.Maybe

import Network.IRC.Message

type Channel = ByteString
type Server  = ByteString

data Command = Command Server Cmd [Param] (Maybe Param)

instance Show Command where
    show = unpack . showCommand

showCommand :: Command -> ByteString
showCommand (Command _ cmd params trail) = cmd `append` " " `append` unwords params `append` " " `append` maybe empty (cons ':') trail

commandDestination :: Command -> Server
commandDestination (Command server _ _ _) = server

setNickname :: Server -> Name -> Command
setNickname server name = Command server "NICK" [name] Nothing

setUsername :: Server -> Name -> Name -> Command
setUsername server name0 name1 = Command server "USER" [name0, "0", "*"] (Just name1)


raw :: Server -> Cmd -> [Param]-> Maybe Param -> Command
raw = Command

pong :: Server -> ByteString -> Command
pong server stamp = Command server "PONG" [] (Just stamp)

joinChannel :: Server -> Channel -> Command
joinChannel server channel = raw server "JOIN" [channel] Nothing

putChannel :: Server -> Channel -> ByteString -> Command
putChannel server channel msg = raw server "PRIVMSG" [channel] (Just msg)





{-
 
showMessage :: Message -> T.Text
showMessage msg = prefix (origin msg) `T.append` T.unwords (cmd msg : param msg) `T.append` "\r\n"
    where
        prefix (Just (Server a))            = ':' `T.cons` a `T.append` " "
        prefix (Just (Nickname n (Just h))) = ':' `T.cons` n `T.append` "!~" `T.append` h `T.append` " "
        prefix (Just (Nickname n Nothing))  = ':' `T.cons` n `T.append` " "
        prefix Nothing                      = T.empty

        params [] = []
        params x  = init x ++ [':' `T.cons` last x]















-}
