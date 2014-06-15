{-# LANGUAGE OverloadedStrings #-}

module Network.IRC
  ( IRCClient(..)
  , Name
  , Hostname
  , Channel
  , Cmd
  , Param
  , testClient
  ) where

import Data.Text

type Hostname = Text
type Name     = Text
type Channel  = Text
type Cmd      = Text
type Param    = Text

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
  , channels = ["#felixsch","#fde"]
  , nickname = "blublubbot"
  , realname = "felixsch irc bot"
  }




