{-# LANGUAGE OverloadedStrings #-}

module Network.IRC.Message
  ( Origin(..)
  , Message(..)
  , Hostname
  , Name
  , Param
  , showMessage
  , parseMessage
  ) where

import Prelude hiding (takeWhile, unwords, words)

import Control.Monad

import Control.Applicative hiding (empty)

import Data.Char (isUpper, isDigit)

import qualified Data.Text as T
import Data.Attoparsec.Text


type Hostname = T.Text
type Name     = T.Text
type Param    = T.Text


data Origin = Host Hostname
            | Nickname Name (Maybe Hostname)

data Message = Message
  { msgOrigin :: Maybe Origin
  , msgCommand   :: T.Text
  , msgParams :: [Param] }




instance Show Message where
    show msg = T.unpack $ T.intercalate "," $ msgParams msg

showMessage :: Message -> T.Text
showMessage msg = prefix (msgOrigin msg) `T.append` T.unwords (msgCommand msg : msgParams msg)
    where
        prefix (Just (Host a))            = ':' `T.cons` a `T.append` " "
        prefix (Just (Nickname n (Just h))) = ':' `T.cons` n `T.append` "!~" `T.append` h `T.append` " "
        prefix (Just (Nickname n Nothing))  = ':' `T.cons` n `T.append` " "
        prefix Nothing                      = T.empty


parseMessage :: T.Text -> Maybe Message
parseMessage = fromEither . parseOnly parser
    where
        parser = Message <$> parseOrigin <*> parseCommand <*> (toWords <$> parseParams)
        fromEither (Left _)    = Nothing
        fromEither (Right msg) = Just msg
        toWords = concatMap T.words

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
      maybeHost = option Nothing $ Just <$> (many (char '~') *> takeTill (== ' ') <* ws)

parseCommand :: Parser T.Text
parseCommand = (takeWhile1 isUpper <|> takeWhile1 isDigit) <* ws

parseParams :: Parser [T.Text]
parseParams = many (end <|> str)
    where
        end = colon *> takeWhile (/= '\r') <* le
        str = takeWhile isOk <* (ws <|> le)
        
        isOk '\n' = False
        isOk '\r' = False
        isOk ' '  = False
        isOk _    = True

