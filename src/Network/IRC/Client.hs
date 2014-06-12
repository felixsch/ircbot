{-# LANGUAGE OverloadedStrings #-}

module Network.IRC.Client
  ( IRCClient(..)
  , Channel
  , testClient
  ) where

import Data.Text


type Channel = Text

data IRCClient = IRCClient 
  { server :: String
  , port :: Int
  , channels :: [Text]
  , nickname :: Text
  , realname :: Text
  }


testClient :: IRCClient
testClient = IRCClient
  { server = "irc.freenode.org"
  , port   = 6667
  , channels = ["#felixsch", "#fde"]
  , nickname = "blublubbot"
  , realname = "felixsch irc bot"
  }




