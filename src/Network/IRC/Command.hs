{-# LANGUAGE OverloadedStrings #-}

module Network.IRC.Command
  ( Command(..)
  , showCommand
  , raw
  , join
  , putChannel
  ) where

import Prelude hiding (unwords)

import Data.Text
import Data.Maybe

import Network.IRC

data Command = Command Cmd [Param] (Maybe Param)

instance Show Command where
    show = unpack . showCommand

showCommand :: Command -> Text
showCommand (Command cmd params trail) = cmd `append` " " `append` unwords params `append` " " `append` maybe empty (cons ':') trail


raw :: Cmd -> [Param]-> Maybe Param -> Command
raw = Command


join :: Channel -> Command
join channel = raw "JOIN" [channel] Nothing

putChannel :: Channel -> Text -> Command
putChannel channel msg = raw "PRIVMSG" [channel] (Just msg)





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
