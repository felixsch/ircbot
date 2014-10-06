{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
import Prelude hiding (unwords, words, writeFile)
import Control.Applicative
import Control.Monad.IO.Class
import Control.Monad.State

import Control.Concurrent.MVar

import qualified Data.Text as T
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as BL

import Network.URI
import Network.HTTP.Conduit     ( simpleHttp )

import Network.IRC
import Network.IRC.Action

import Lsvm
import Scheme

ircNick, ircAltNick, ircRealname :: T.Text

ircNick     = "jotesting"
ircAltNick  = ircNick `T.append` "_"
ircRealname = ircAltNick `T.append` "_"


tmpImage, model :: FilePath

tmpImage = "/tmp/ircbot-catscanner"
model    = "cat.xml"


freenode, quakenet :: IrcServer

freenode = IrcServer
    { host     = "irc.freenode.org"
    , port     = 6667
    , channels = []
    , nick     = ircNick
    , altNick  = ircAltNick
    , realName = ircRealname
    , password = Nothing }

quakenet = IrcServer
    { host     = "irc.quakenet.org"
    , port     = 6667
    , channels = []
    , nick     = ircNick
    , altNick  = ircAltNick
    , realName = ircRealname
    , password = Nothing }

data Data = Data
  { recordedWords :: Int
  , images        :: [(T.Text, Bool)]
  , quit          :: MVar Bool
  , admins        :: [Cloak]
  , schemeEnv        :: Env Data }

instance WithPriviliges Data where
    hasPrivilige "admin" cloak = do
        adms <- admins <$> get
        logM "privs" $ cloak `T.append` " == " `T.append` "admin?!"
        return $ cloak `elem` adms

    hasPrivilige _       _     = return False

instance WithScheme Data where
    getEnv = schemeEnv <$> get
    putEnv s = modify (\dat -> dat { schemeEnv = s })

mkData :: IO Data
mkData = do
   trigger <- newEmptyMVar  
   return Data
     { recordedWords  = 0
     , images         = []
     , quit           = trigger
     , admins         = []
     , schemeEnv      = newEnvWith 4000 }
       

runJbot :: IO (Maybe IrcError)
runJbot = do
    dat <- mkData 
    result <- runIrcMonad [quakenet,freenode] dat (defaultIrcLogger True Nothing) (start actions)
    waitUntil $ quit dat
    return result

    where
        actions = countWords >> kittens >> kittenStats 
               >> quitBot 
               >> schemeEval ">" 
               >> schemeGetDefined "!def"
               >> schemeClearState "!clear" (newEnvWith 4000)
        waitUntil = void . readMVar
 
main :: IO ()
main = do
    e <- runJbot
    case e of
        Just err -> print err
        Nothing  -> putStrLn "bye"


quitBot :: Action Data ()
quitBot = whenAdmin $ whenTrigger "!quit" $ \dest _ -> do
    trigger <- quit <$> get
    me dest "is going offline"
    liftIO $ putMVar trigger True
    

countWords :: Action Data ()
countWords = onPrivMsg checkTrigger
    where
        checkTrigger dest (x:xs)
          | x == "?recorded" = showCountedWords dest
          | otherwise     = modify (\dat -> dat { recordedWords = recordedWords dat + (length xs) + 1})

        showCountedWords dest = do
            count <- recordedWords <$> get
            say dest $ "Recorded " `T.append` T.pack (show count) `T.append` " words."
 
kittens :: Action Data ()
kittens = onPrivMsg $ \dest txt ->
    forM_ (concatMap T.words txt) $ \word -> whenURI word $
        case isSupported word of
            Just ty  -> checkImage dest word ty
            Nothing  -> return ()

whenURI :: (MonadIO m) => T.Text -> m () -> m ()
whenURI text = when (isURI $ T.unpack text)


kittenStats :: Action Data ()
kittenStats = whenTrigger "!kittens" $ \dest _ -> do
    dat <- get
    say dest $ "Kittens found : " `T.append` T.pack (show $ countKittens $ images dat)
    say dest $ "total images  : " `T.append` T.pack (show $ Prelude.length $ images dat)
    say dest "last 3 images :"
    forM_ (Prelude.take 3 $ images dat) $ \(url,isCat) ->
        say dest $ if isCat 
            then "  - " `T.append` url `T.append` " (detected as cat)"
            else "  - " `T.append` url
    where
        countKittens ((_,True):xs) = 1 + countKittens xs
        countKittens (_:xs)        = 0 + countKittens xs
        countKittens []            = 0
    

checkImage :: T.Text -> T.Text -> String -> Action Data ()
checkImage dest url ty = do
    imgs  <- images <$> get

    case lookup url imgs of
        Just isCat -> when isCat $ say dest "Uhm I remember this cute kitten.. :)"
        Nothing    -> do
            image <- liftIO $ simpleHttp $ T.unpack url
            liftIO $ BL.writeFile tmp image
 
            isCat <- liftIO $ detectCat tmp model (-0.4) 4

            modify (\dat -> dat { images = (url,isCat) : images dat })
            when isCat $ say dest "Hurray a cute kitten..."
    where
        tmp = tmpImage ++ "." ++ ty


isSupported :: T.Text -> Maybe String
isSupported = isSup . reverse . T.unpack
    where
        isSup ('g':'p':'j':_) = Just "jpg"
        isSup ('g':'n':'p':_) = Just "png"
        isSup ('g':'e':'p':'j':_) = Just "jpeg"
        isSup _               = Nothing
