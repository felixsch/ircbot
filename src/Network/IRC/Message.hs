{-# LANGUAGE OverloadedStrings #-}

module Network.IRC.Message
  ( Origin(..)
  , Message(..)
  , showMessage
  , parseMessage
  ) where

import Prelude hiding (takeWhile, unwords)

import Control.Monad
import Control.Monad.IO.Class

import Control.Applicative hiding (empty)

import qualified Data.Text as T
import Data.Char (isUpper, isDigit)
import Data.Conduit
import Data.Attoparsec.Text

import Network.IRC

data Origin = Server Hostname
            | Nickname Name (Maybe Hostname)
            deriving (Show, Eq)

data Message = Message 
  { origin :: Maybe Origin
  , cmd    :: Cmd
  , params :: [Param] }


instance Show Message where
    show = T.unpack . showMessage

showMessage :: Message -> T.Text
showMessage msg = prefix (origin msg) `T.append` T.unwords (cmd msg : params msg) `T.append` "\r\n"
    where
        prefix (Just (Server a))            = ':' `T.cons` a `T.append` " "
        prefix (Just (Nickname n (Just h))) = ':' `T.cons` n `T.append` "!~" `T.append` h `T.append` " "
        prefix (Just (Nickname n Nothing))  = ':' `T.cons` n `T.append` " "
        prefix Nothing                      = T.empty



parseMessage :: (MonadIO m) => Conduit T.Text m Message
parseMessage = awaitForever $ \t -> 
    case parseOnly attoParseMessage t of
        Left _ -> return ()
        Right msg -> yield msg


attoParseMessage :: Parser Message
attoParseMessage = Message <$> parseOrigin <*> parseCommand <*> parseParams

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
parseHost = Server <$ colon <*> host <* ws
    where
        host = takeWhile (/= ' ')

parseNickname :: Parser Origin
parseNickname = Nickname <$ colon <*> name <*> maybeHost
    where
      name = takeTill (`elem` " .!") <* (bang <|> ws)
      maybeHost = option Nothing $ Just <$> (char '~' *> takeTill (== ' ') <* ws)

parseCommand :: Parser T.Text
parseCommand = (takeWhile1 isUpper <|> takeWhile1 isDigit) <* ws

parseParams :: Parser [T.Text]
parseParams = many1 (end <|> str)
    where
        end = colon *> (T.replace "\r\n" T.empty <$> takeText)
        str = takeWhile (/= ' ') <* ws



