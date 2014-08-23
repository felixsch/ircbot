{-# LANGUAGE OverloadedStrings #-}

import Prelude hiding (unwords, words, writeFile)
import Control.Monad.IO.Class
import Control.Monad
import Network.URI
import Network.HTTP.Conduit hiding (host, port)
import Data.ByteString.Char8 hiding (putStrLn, reverse, concatMap)
import qualified Data.ByteString.Lazy as BL
import Network.IRC
import Network.IRC.Command
import Network.IRC.Message


import Data.Conduit
import qualified Data.Conduit.List as CL


import Lsvm



ircNick, ircAltNick, ircRealname :: ByteString
ircNick     = pack "jobotos"
ircAltNick  = ircNick `append` "_"
ircRealname = ircAltNick `append` "_"

freenode, quakenet :: IrcServerSettings
freenode = IrcServerSettings
    { host     = "irc.freenode.org"
    , port     = 6667
    , channels = ["#felixsch", "#moepmoepmoep"]
    , nick     = ircNick
    , altNick  = ircAltNick
    , realName = ircRealname }
quakenet = IrcServerSettings
    { host     = "irc.quakenet.org"
    , port     = 6667
    , channels = ["#felixsch"]
    , nick     = ircNick
    , altNick  = ircAltNick
    , realName = ircRealname }


myConfig = IrcConfig
  { servers = [freenode, quakenet]
  , verbose = True
  }

myActions = felixsch >> kittens

main :: IO ()
main = do
    error <- runIRC myConfig myActions
    case error of
        Just err -> putStrLn $ show err
        Nothing  -> putStrLn "bye"

felixsch :: Action ()
felixsch = onChannel "#felixsch" $ \params ->
    say "#felixsch" $ "You said: " `append` unwords params
    
kittens :: Action ()
kittens = onPrivMsg $ \dest txt -> do
    forM_ (concatMap words txt) $ \word -> whenURI word $ do
        case isSupported word of
            Just ty  -> checkImage dest word ty
            otherwise -> return ()

whenURI :: (MonadIO m) => ByteString -> (m ()) -> m ()
whenURI text cb = do
    if isURI $ unpack text
        then cb
        else return ()

tmpImage = "/tmp/ircbot-catscanner"
model    = "/mnt/files/git/ircbot/cat.xml"

checkImage :: ByteString -> ByteString -> String -> Action ()
checkImage dest url ty = do
    image <- liftIO $ simpleHttp $ unpack url
    liftIO $ BL.writeFile tmp image

    isACat <- liftIO $ detectCat tmp model (-0.4) 4

    if isACat
        then say dest "Hurray a cute kitten..."
        else return ()
    where
        tmp = tmpImage ++ "." ++ ty


isSupported :: ByteString -> Maybe String
isSupported = isSup . reverse . unpack
    where
        isSup ('g':'p':'j':_) = Just "jpg"
        isSup ('g':'n':'p':_) = Just "png"
        isSup ('g':'e':'p':'j':_) = Just "jpeg"
        isSup _               = Nothing
