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
  { server = "irc.quakenet.org"
  , port   = 6667
  , channels = ["#hacky.v2", "#hAcky.v2"]
  , nickname = "joogisb"
  , realname = "moep bot moep"
  }




