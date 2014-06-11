{-# LANGUAGE OverloadedStrings #-}

module Network.IRC.Message
  ( IRCMessage(..)
  , IRCMessagePrefix(..)
  , Hostname
  , Nick
  , decodeMessage
  , encodeMessage
  , attoParseMessage
  ) where

import Prelude hiding (takeWhile, unwords)
import Control.Monad
import Control.Monad.Catch hiding (try)
import Control.Applicative hiding (empty)

import qualified Data.Text as T
import Data.Char (isUpper, isDigit)
import Data.Conduit
import qualified Data.Conduit.List as CL
import Data.Conduit.Attoparsec

import Data.Attoparsec.Text

type Hostname = T.Text
type Nick     = T.Text

data IRCMessagePrefix = Server Hostname
                      | Nickname Nick (Maybe Hostname)
                      deriving (Show)

data IRCMessage = IRCMessage
  { msgPrefix :: Maybe IRCMessagePrefix
  , msgCommand :: T.Text
  , msgParams :: [T.Text]
  } deriving (Show)


showMessage :: IRCMessage -> T.Text
showMessage msg = T.unwords $ [prefix (msgPrefix msg), msgCommand msg] ++ params (msgParams msg)
    where
        prefix (Just (Server a))            = ':' `T.cons` a
        prefix (Just (Nickname n (Just h))) = ':' `T.cons` n `T.append` "!~" `T.append` h
        prefix (Just (Nickname n Nothing))  = ':' `T.cons` n
        prefix Nothing                      = T.empty

        params [] = []
        params x  = init x ++ [':' `T.cons` last x]


decodeMessage :: (MonadThrow m) => Consumer T.Text m IRCMessage
decodeMessage = sinkParser attoParseMessage

encodeMessage :: (Monad m) => Conduit IRCMessage m T.Text
encodeMessage = CL.map showMessage

attoParseMessage :: Parser IRCMessage
attoParseMessage = IRCMessage <$> parsePrefix <*> parseCommand <*> parseParams

colon :: Parser Char
colon = char ':'

bang :: Parser ()
bang = void $ char '!'

ws :: Parser ()
ws = void $ char ' '

parsePrefix :: Parser (Maybe IRCMessagePrefix)
parsePrefix = option Nothing $ Just <$> (try parseNickname <|> parseHost)

parseHost :: Parser IRCMessagePrefix
parseHost = Server <$ colon <*> host <* ws
    where
        host = takeWhile (/= ' ')

parseNickname :: Parser IRCMessagePrefix
parseNickname = Nickname <$ colon <*> name <*> maybeHost
    where
      name = takeTill (`elem` " .!") <* (bang <|> ws)
      maybeHost = option Nothing $ Just <$> (char '~' *> takeTill (== ' ') <* ws)

parseCommand :: Parser T.Text
parseCommand = (takeWhile1 isUpper <|> takeWhile1 isDigit) <* ws

parseParams :: Parser [T.Text]
parseParams = many1 (end <|> str)
    where
        end = colon *> takeText
        str = takeWhile (/= ' ') <* ws


    


