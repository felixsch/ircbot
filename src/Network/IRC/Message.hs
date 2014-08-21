{-# LANGUAGE OverloadedStrings #-}

module Network.IRC.Message
  ( Origin(..)
  , Message(..)
  , Hostname
  , Name
  , Cmd
  , Param
  , showMessage
  , parseMessage
  ) where

import Prelude hiding (takeWhile, unwords)

import Control.Monad
import Control.Monad.IO.Class

import Control.Applicative hiding (empty)

import Data.Char (isUpper)
import Data.ByteString.Char8 hiding (takeWhile, elem)
import Data.Attoparsec.ByteString.Char8


type Hostname = ByteString
type Name     = ByteString
type Cmd      = ByteString
type NumCmd   = Int
type Param    = ByteString




data Host = Host Hostname
data Nickname = Nickname Name (Maybe Hostname)

data Message = PingMsg ByteString
             | ChannelMsg Channel Nickname [Param]
             | PrivMsg Nickname [Param]
             | NoticeMsg Nickname [Param]
             | NumMsg Host NumCmd [Param]
             | Message
                { msgOrigin :: Maybe (Either Host Nickname)
                , msgCommand   :: Cmd
                , msgParams :: [Param] }

toGenericMessage :: Message -> Message
toGenericMessage (Message Nothing "PING" (x:_)) = PingMsg x
toGenericMessage (Message (Just (Left user)) "PRIVMSG" ((chan@'#':_):params) = ChannelMsg chan user params
toGenericMessage (Message (Just (Left user)) "PRIVMSG" (_:params)) = PrivMsg user params

toGenericMessage (Message (Just (Right host)) num params) = NumMsg host cmd


fromGenericMessage :: Message -> Maybe Message



instance Show Message where
    show = unpack . showMessage

showMessage :: Message -> ByteString
showMessage msg = prefix (origin msg) `append` unwords (cmd msg : params msg)
    where
        prefix (Just (Host a))            = ':' `cons` a `append` " "
        prefix (Just (Nickname n (Just h))) = ':' `cons` n `append` "!~" `append` h `append` " "
        prefix (Just (Nickname n Nothing))  = ':' `cons` n `append` " "
        prefix Nothing                      = empty


parseMessage :: ByteString -> Maybe Message
parseMessage = fromEither . parseOnly parser
    where
        parser = Message <$> parseOrigin <*> parseCommand <*> parseParams
        fromEither (Left _)    = Nothing
        fromEither (Right msg) = Just msg

colon :: Parser Char
colon = char ':'

bang :: Parser ()
bang = void $ char '!'

ws :: Parser ()
ws = void $ char ' '

le :: Parser ()
le = void $ char '\r'

parseOrigin :: Parser (Maybe Origin)
parseOrigin = option Nothing $ Just <$> (try parseNickname <|> parseHost)

parseHost :: Parser Origin
parseHost = Host <$ colon <*> host <* ws
    where
        host = takeWhile (/= ' ')

parseNickname :: Parser Origin
parseNickname = Nickname <$ colon <*> name <*> maybeHost
    where
      name = takeTill (`elem` " .!") <* (bang <|> ws)
      maybeHost = option Nothing $ Just <$> (char '~' *> takeTill (== ' ') <* ws)

parseCommand :: Parser ByteString
parseCommand = (takeWhile1 isUpper <|> takeWhile1 isDigit) <* ws

parseParams :: Parser [ByteString]
parseParams = many (end <|> str)
    where
        end = colon *> takeWhile (/= '\r') <* le
        str = takeWhile isOk <* (ws <|> le)
        
        isOk '\n' = False
        isOk '\r' = False
        isOk ' '  = False
        isOk _    = True

