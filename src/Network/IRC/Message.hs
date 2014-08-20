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
type Param    = ByteString

data Origin = Host Hostname
            | Nickname Name (Maybe Hostname)
            deriving (Show, Eq)

data Message = Message 
  { origin :: Maybe Origin
  , cmd    :: Cmd
  , params :: [Param] }


instance Show Message where
    show = unpack . showMessage

showMessage :: Message -> ByteString
showMessage msg = prefix (origin msg) `append` unwords (cmd msg : params msg) `append` "\r\n"
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

newline :: Parser ()
newline = void $ char '\n'

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
parseParams = many1 (end <|> str) <* char '\r' <* char '\n'
    where
        end = colon *> takeWhile (/= '\r')
        str = takeWhile isOk <* ws
        
        isOk '\n' = False
        isOk '\r' = False
        isOk ' '  = False
        isOk _    = True

