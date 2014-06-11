{-# LANGUAGE OverloadedStrings #-}

module Network.IRC.Message where
--  ( IRCMessage(..)
--  , parseMessage
--  , attoParseMessage
--  ) where

import Prelude hiding (takeWhile)
import Control.Monad
import Control.Monad.Catch hiding (try)
import Control.Applicative 

import Data.Text (Text)
import Data.Char (isUpper, isDigit)
import Data.Conduit
import Data.Conduit.Attoparsec

import Data.Attoparsec.Text

data IRCMessagePrefix = Server Text
                      | Nickname Text (Maybe Text)
                      deriving (Show)

data IRCMessage = IRCMessage
  { msgPrefix :: Maybe IRCMessagePrefix
  , msgCommand :: Text
  , msgParams :: [Text]
  } deriving (Show)

parseMessage :: (MonadThrow m) => Consumer Text m IRCMessage
parseMessage = sinkParser attoParseMessage

attoParseMessage :: Parser IRCMessage
attoParseMessage = IRCMessage <$> parsePrefix <*> parseCommand <*> parseParams

colon :: Parser Char
colon = char ':'

ws :: Parser ()
ws = void $ char ' '

parsePrefix :: Parser (Maybe IRCMessagePrefix)
parsePrefix = option Nothing $ Just <$> (try parseHost <|> parseNickname)

parseHost :: Parser IRCMessagePrefix
parseHost = Server <$ colon <*> host <* ws
    where
        host = takeWhile (\x -> x /= ' '
                             || x `notElem` "!~@")

parseNickname :: Parser IRCMessagePrefix
parseNickname = Nickname <$ colon <*> name <*> maybeHost <* ws
    where
      name = takeTill (\x -> x == ' '
                          || x == '!')
      maybeHost = option Nothing $ Just <$> (string "!~" *> takeTill (== ' '))

parseCommand :: Parser Text
parseCommand = (takeWhile1 isUpper <|> takeWhile1 isDigit) <* ws

parseParams :: Parser [Text]
parseParams = many1 (end <|> str)
    where
        end = colon *> takeText
        str = takeWhile (/= ' ') <* ws


    


