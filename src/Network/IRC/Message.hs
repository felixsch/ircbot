{-# LANGUAGE OverloadedStrings #-}

module Network.IRC.Message
  ( Origin(..)
  , Message(..)
  , fromGenericMessage
  , toGenericMessage
  , isPingMsg
  , isChannelMsg
  , isPrivMsg
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


type Channel  = ByteString
type Hostname = ByteString
type Name     = ByteString
type Cmd      = ByteString
type NumCmd   = ByteString
type Param    = ByteString


data Host     = Host Hostname
data Nickname = Nickname Name (Maybe Hostname)

type Origin   = Either Host Nickname

data Message = PingMsg ByteString
             | ChannelMsg Channel Nickname [Param]
             | PrivMsg Nickname [Param]
             | NumMsg Host NumCmd [Param]
             | Message
                { msgOrigin :: Maybe Origin
                , msgCommand   :: Cmd
                , msgParams :: [Param] }

isChannel :: ByteString -> Bool
isChannel = check . unpack
    where
        check ('#':_) = True
        check _       = False

toGenericMessage :: Message -> Message
toGenericMessage (Message Nothing "PING" (x:_))                               = PingMsg x
toGenericMessage (Message (Just (Left host)) num params)                     = NumMsg host num params
toGenericMessage (Message (Just (Right user)) "PRIVMSG" (x:xs))
    | isChannel x = ChannelMsg x user xs
    | otherwise      = PrivMsg user xs
toGenericMessage x                                                            = x


fromGenericMessage :: Message -> Message
fromGenericMessage (PingMsg stamp)               = Message Nothing "PING" [stamp]
fromGenericMessage (ChannelMsg chan user params) = Message (Just $ Right user) "PRIVMSG" $ chan : params
fromGenericMessage (PrivMsg user params)         = Message (Just $ Right user) "PRIVMSG" params
fromGenericMessage (NumMsg host cmd params)      = Message (Just $ Left host) cmd params
fromGenericMessage x                             = x

isPingMsg :: Message -> Bool
isPingMsg (PingMsg _)  = True
isPingMsg _            = False

isChannelMsg :: Message -> Bool
isChannelMsg (ChannelMsg _ _ _) = True
isChannelMsg _              = False

isPrivMsg :: Message -> Bool 
isPrivMsg (PrivMsg _ _) = True
isPrivMsg _           = False


instance Show Message where
    show = unpack . showMessage

showMessage :: Message -> ByteString
showMessage (Message origin cmd params) = prefix origin `append` unwords (cmd : params)
    where
        prefix (Just (Left (Host a)))               = ':' `cons` a `append` " "
        prefix (Just (Right (Nickname n (Just h)))) = ':' `cons` n `append` "!~" `append` h `append` " "
        prefix (Just (Right (Nickname n Nothing)))  = ':' `cons` n `append` " "
        prefix Nothing                              = empty
showMessage x                           = showMessage $ toGenericMessage x


parseMessage :: ByteString -> Maybe Message
parseMessage = fromEither . parseOnly parser
    where
        parser = Message <$> parseOrigin <*> parseCommand <*> parseParams
        fromEither (Left _)    = Nothing
        fromEither (Right msg) = Just $ fromGenericMessage msg

colon :: Parser Char
colon = char ':'

bang :: Parser ()
bang = void $ char '!'

ws :: Parser ()
ws = void $ char ' '

le :: Parser ()
le = void $ char '\r'

parseOrigin :: Parser (Maybe Origin)
parseOrigin = option Nothing $ Just <$> (try (Right <$> parseNickname) <|> (Left <$> parseHost))

parseHost :: Parser Host
parseHost = Host <$ colon <*> host <* ws
    where
        host = takeWhile (/= ' ')

parseNickname :: Parser Nickname
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

