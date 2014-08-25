{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
import Prelude hiding (unwords, words, writeFile)
import Control.Applicative
import Control.Monad.IO.Class
import Control.Monad.State
import Control.Monad.Base
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

data JData = JData 
  { lastSentence :: ByteString
  , kittensFound :: Int
  , images       :: [ByteString]
  }

mkJData :: JData
mkJData = JData
  { lastSentence = "blll"
  , kittensFound = 0
  , images       = []
}


instance MonadIrc (StateT JData IO)



lastS :: Action (StateT JData IO) ()
lastS = onPrivMsg checkTrigger
    where
        checkTrigger dest (x:xs)
          | x == "!last" = showLast dest
          | otherwise    = saveLast $ x:xs

        showLast dest    = do        
            state <- lift get 
            say dest $ "Last sentence recorded: " `append` lastSentence state

        saveLast params  = do
            state <- lift get
            irc $ ircLog Nothing $ "params are: " ++ unpack (intercalate ", " params)
            lift $ put (state { lastSentence = unwords params })
            state2 <- lift get
            irc $ ircLog Nothing $ "params are: " ++ unpack (lastSentence state2)

paramTest :: Action (StateT JData IO) ()
paramTest = whenTrigger "!params" $ \dest params ->
    say dest $ "Params where: " `append` intercalate ", " params


runJbot :: IO (Maybe IrcError)
runJbot = from <$> (runStateT (runIRC myConfig actions) mkJData)
    where
        actions = lastS >> paramTest >> kittens >> kittenStats >> hurray
        from (x, _) = x
    

hurray :: Action (StateT JData IO) ()
hurray = whenTrigger "!hurray" $ \dest _ -> do
    hur <- test <$> (irc $ get)
    say dest $ "Hurray: " `append` hur

main :: IO ()
main = do
    error <- runJbot
    case error of
        Just err -> putStrLn $ show err
        Nothing  -> putStrLn "bye"

    
kittens :: Action (StateT JData IO) ()
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

kittenStats :: Action (StateT JData IO) ()
kittenStats = whenTrigger "!kittens" $ \dest params -> do
    dat <- lift $ get
    say dest $ "Kittens found : " `append` (pack $ show $ kittensFound dat)
    say dest $ "total images  : " `append` (pack $ show $ Prelude.length $ images dat)
    say dest $ "last 5 images :"
    forM_ (Prelude.take 5 $ images dat) $ \url ->
        say dest $ "  - " `append` url
    

checkImage :: ByteString -> ByteString -> String -> Action (StateT JData IO) ()
checkImage dest url ty = do
    image <- liftIO $ simpleHttp $ unpack url
    liftIO $ BL.writeFile tmp image

    
    lift $ modify (\dat -> dat { images = url : images dat })
    isACat <- liftIO $ detectCat tmp model (-0.4) 4

    if isACat
        then do
            say dest "Hurray a cute kitten..."
            lift $ modify (\dat -> dat { kittensFound = kittensFound dat + 1 })
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
