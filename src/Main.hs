{-# LANGUAGE OverloadedStrings #-}
import Control.Monad.IO.Class
import Control.Monad
import Network.URI
import qualified Data.ByteString.Lazy as B
import Network.HTTP.Conduit
import Network.IRC
import Network.IRC.Conduit
import Network.IRC.Command
import Network.IRC.Message

import Data.Conduit
import qualified Data.Conduit.List as CL
import qualified Data.Text as T


import Lsvm

main :: IO ()
main = runIRC testClient (onPing =$= theKitten =$= logIRC)

logIRC :: (MonadIO m) => IRCConduit m
logIRC = awaitForever handleIRC
    where
      handleIRC s@(Left msg) = (liftIO $ print msg)
      handleIRC s@(Right c) = (liftIO $ putStrLn $ "> " ++ show c) >> yield s


onPing :: (MonadIO m) => IRCConduit m
onPing = onAction "PING" $ do
    msg <- await
    case msg of
      Just m -> pong m
      Nothing -> return ()
    where
     pong (Left (Message _ _ (x:_))) = send $ raw "PONG" [':' `T.cons` x] Nothing
     pong x                           = return ()


onFelixsch :: (MonadIO m) => IRCConduit m
onFelixsch = do
    msg <- await
    case msg of
        Just (Left (Message _ _ pa)) -> send $ raw "PRIVMSG" ["#felixsch"] (Just $ "echo: " `T.append` getMsg pa)
        otherwise                    -> return ()
    where
      getMsg [] = T.empty
      getMsg x  = last x

theKitten :: (MonadIO m) => IRCConduit m
theKitten = onAction "PRIVMSG" $ awaitForever $ manageKittens
    where
        manageKittens m@(Left (Message _ _ (chan:txt))) = mapM_ (handleParam m chan) txt    
        manageKittens x = yield x

        handleParam m chan txt = forM_ (T.words txt) $ \msg -> whenURI msg $ do
                case isSupported msg of
                    Just ty   -> checkImage chan ty msg >> yield m
                    otherwise -> yield m

whenURI :: (MonadIO m) => T.Text -> (m ()) -> m ()
whenURI text cb = do
    if isURI $ T.unpack text
        then cb
        else return ()

tmpImage = "/tmp/ircbot-catscanner"
model    = "/mnt/files/git/ircbot/cat.xml"

checkImage :: (MonadIO m) => Channel -> String -> T.Text -> IRCConduit m
checkImage chan ty imageURL = do
    image <- liftIO $ simpleHttp $ T.unpack imageURL
    liftIO $ B.writeFile tmp image

    isACat <- liftIO $ detectCat tmp model (-0.4) 4

    if isACat
        then send $ putChannel chan "Hurray a cute kitten..."
        else return ()
    where
        tmp = tmpImage ++ "." ++ ty


isSupported :: T.Text -> Maybe String
isSupported = isSup . reverse . T.unpack
    where
        isSup ('g':'p':'j':_) = Just "jpg"
        isSup ('g':'n':'p':_) = Just "png"
        isSup _               = Nothing
