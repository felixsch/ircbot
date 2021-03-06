{-# LANGUAGE OverloadedStrings #-}

module Network.IRC.Command
  ( Cmd(..)
  , Channel
  , Server
  , showCmd
  , cmdDestination
  , raw
  , mkPong
  , mkJoin
  , mkPut
  , mkNotice
  , mkMe
  , setNickname
  , setUsername
  ) where

import Prelude hiding (unwords)

import qualified Data.Text as T

import Network.IRC.Message

type Destination = T.Text
type Channel = T.Text
type Server  = T.Text



data Cmd = Cmd Server T.Text [Param] (Maybe Param)

instance Show Cmd where
    show = T.unpack . showCmd

showCmd :: Cmd -> T.Text
showCmd (Cmd _ cmd params trail) = cmd `T.append` " " `T.append` T.unwords params `T.append` " " `T.append` maybe T.empty (T.cons ':') trail

cmdDestination :: Cmd -> Server
cmdDestination (Cmd server _ _ _) = server

setNickname :: Server -> Name -> Cmd
setNickname server name = Cmd server "NICK" [name] Nothing

setUsername :: Server -> Name -> Name -> Cmd
setUsername server name0 name1 = Cmd server "USER" [name0, "0", "*"] (Just name1)


raw :: Server -> T.Text -> [Param]-> Maybe Param -> Cmd
raw = Cmd

mkPong :: Server -> T.Text -> Cmd
mkPong server stamp = Cmd server "PONG" [] (Just stamp)

mkJoin :: Server -> Channel -> Cmd
mkJoin server channel = raw server "JOIN" [channel] Nothing

mkPut :: Server -> Destination -> T.Text -> Cmd
mkPut server channel msg = raw server "PRIVMSG" [channel] (Just msg)

mkNotice :: Server -> Name -> T.Text -> Cmd
mkNotice server nick msg = raw server "NOTICE" [nick] (Just msg)

mkMe :: Server -> Destination -> T.Text -> Cmd
mkMe server dest msg = raw server ('\x01' `T.cons` "ACTION") [dest, msg, "\x01"] Nothing






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
