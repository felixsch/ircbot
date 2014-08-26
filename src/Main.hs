{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
import Prelude hiding (unwords, words, writeFile)
import Control.Applicative
import Control.Monad.IO.Class
import Control.Monad.State
import Control.Monad.Base
import Control.Monad

import Control.Concurrent.MVar

import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as BL

import Network.URI
import Network.HTTP.Conduit     ( simpleHttp )

import Network.IRC
import Network.IRC.Action

import Lsvm


ircNick     = "jobotos"
ircAltNick  = ircNick `B.append` "_"
ircRealname = ircAltNick `B.append` "_"

freenode = IrcServer
    { host     = "irc.freenode.org"
    , port     = 6667
    , channels = ["#felixsch", "#moepmoepmoep"]
    , nick     = ircNick
    , altNick  = ircAltNick
    , realName = ircRealname
    , password = Nothing }

quakenet = IrcServer
    { host     = "irc.quakenet.org"
    , port     = 6667
    , channels = ["#felixsch", "#hacky.v2"]
    , nick     = ircNick
    , altNick  = ircAltNick
    , realName = ircRealname
    , password = Nothing }


data JData = JData 
  { lastSentence :: B.ByteString
  , kittensFound :: Int
  , images       :: [B.ByteString]
  , quitTrigger  :: MVar Bool }

mkJData :: IO JData
mkJData = do
   trigger <- newEmptyMVar  
   return JData
     { lastSentence = "blll"
     , kittensFound = 0
     , images       = []
     , quitTrigger  = trigger }

runJbot :: IO (Maybe IrcError)
runJbot = do
    jdata <- mkJData 
    result <- runIrcMonad [quakenet,freenode] jdata (defaultIrcLogger True Nothing) (start actions)
    waitUntil $ quitTrigger jdata
    return result

    where
        actions = lastS >> paramTest >> kittens >> kittenStats
        waitUntil = void . readMVar
 
main :: IO ()
main = do
    error <- runJbot
    case error of
        Just err -> putStrLn $ show err
        Nothing  -> putStrLn "bye"
lastS :: Action JData ()
lastS = onPrivMsg checkTrigger
    where
        checkTrigger dest (x:xs)
          | x == "!last" = showLast dest
          | otherwise    = saveLast $ x:xs

        showLast dest    = do        
            state <- get 
            say dest $ "Last sentence recorded: " `B.append` lastSentence state

        saveLast params  = do
            state <- get
            logM "lastS" $ "params are: " `B.append` B.intercalate ", " params
            modify (\state -> state { lastSentence = B.unwords params })

paramTest :: Action JData ()
paramTest = whenTrigger "!params" $ \dest params ->
    say dest $ "Params where: " `B.append` B.intercalate ", " params
    
kittens :: Action JData ()
kittens = onPrivMsg $ \dest txt -> do
    forM_ (concatMap B.words txt) $ \word -> whenURI word $ do
        case isSupported word of
            Just ty  -> checkImage dest word ty
            otherwise -> return ()

whenURI :: (MonadIO m) => B.ByteString -> (m ()) -> m ()
whenURI text cb = do
    if isURI $ B.unpack text
        then cb
        else return ()

tmpImage = "/tmp/ircbot-catscanner"
model    = "/mnt/files/git/ircbot/cat.xml"

kittenStats :: Action JData ()
kittenStats = whenTrigger "!kittens" $ \dest _ -> do
    dat <- get
    nick <- currentUser
    notice nick $ "Kittens found : " `B.append` (B.pack $ show $ kittensFound dat)
    notice nick $ "total images  : " `B.append` (B.pack $ show $ Prelude.length $ images dat)
    notice nick $ "last 5 images :"
    forM_ (Prelude.take 5 $ images dat) $ \url ->
        notice nick $ "  - " `B.append` url
    

checkImage :: B.ByteString -> B.ByteString -> String -> Action JData ()
checkImage dest url ty = do
    image <- liftIO $ simpleHttp $ B.unpack url
    liftIO $ BL.writeFile tmp image

    
    modify (\dat -> dat { images = url : images dat })
    isACat <- liftIO $ detectCat tmp model (-0.4) 4

    if isACat
        then do
            say dest "Hurray a cute kitten..."
            modify (\dat -> dat { kittensFound = kittensFound dat + 1 })
        else return ()
    where
        tmp = tmpImage ++ "." ++ ty


isSupported :: B.ByteString -> Maybe String
isSupported = isSup . reverse . B.unpack
    where
        isSup ('g':'p':'j':_) = Just "jpg"
        isSup ('g':'n':'p':_) = Just "png"
        isSup ('g':'e':'p':'j':_) = Just "jpeg"
        isSup _               = Nothing
