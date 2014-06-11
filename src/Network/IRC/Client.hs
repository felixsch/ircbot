module Network.IRC.Client
  ( IRCClient(..)
  ) where



data IRCClient = IRCClient 
  { server :: String
  , port :: Int
  , channels :: [String]
  , nickname :: String
  , realname :: String
  }
