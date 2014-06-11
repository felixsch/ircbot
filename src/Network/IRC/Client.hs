module Network.IRC.Client
  ( IRCClient(..)
  , Channel
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




