{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
import Control.Applicative
import Control.Monad.IO.Class
import Control.Monad.State

import Control.Concurrent.MVar

import qualified Data.Text as T

import Network.IRC
import Network.IRC.Action

import Module.CatDetection
import Module.Scheme

ircNick, ircAltNick, ircRealname :: T.Text

ircNick     = "exampleBot"
ircAltNick  = ircNick `T.append` "_"
ircRealname = ircAltNick `T.append` "_"


freenode :: IrcServer
freenode = IrcServer
    { host     = "irc.freenode.org"
    , port     = 6667
    , channels = ["#felixsch"]
    , nick     = ircNick
    , altNick  = ircAltNick
    , realName = ircRealname
    , password = Nothing }


data ExampleBot = ExampleBot
  { recordedWords :: Int
  , quit          :: MVar Bool
  , admins        :: [Cloak]
  , schemeEnv     :: Env ExampleBot
  , catEnv        :: CatEnv ExampleBot }

instance WithPriviliges ExampleBot where
    hasPrivilige "admin" cloak = do
        adms <- admins <$> get
        logM "privs" $ cloak `T.append` " == " `T.append` "admin?!"
        return $ cloak `elem` adms

    hasPrivilige _       _     = return False

instance WithScheme ExampleBot where
    getEnv = schemeEnv <$> get
    putEnv s = modify (\dat -> dat { schemeEnv = s })

instance WithCatDetection ExampleBot where
    catTmpImage   = return "/tmp/ircbot-catscanner"
    catModel      = return "cat.xml"
    catFoundMsg   = return "Awww I love cats. What a cute one!"
    catRememberMsg = return "Ah I remember this kitten"

    catGetEnv     = catEnv <$> get
    catSetEnv s   = modify (\env -> env { catEnv = s})


initExampleBot :: IO ExampleBot
initExampleBot = do
   trigger <- newEmptyMVar  
   return ExampleBot
     { recordedWords  = 0
     , quit           = trigger
     , admins         = []
     , schemeEnv      = newEnvWith 4000
     , catEnv         = newCatEnv }


runExampleBot :: ExampleBot -> IO (Maybe IrcError)
runExampleBot bot = do
    status <- runIrcMonad [freenode] bot (defaultIrcLogger True Nothing) (start exampleBotBehaviour)
    waitUntil $ quit bot
    return status
  where
      waitUntil = void . readMVar

exampleBotBehaviour :: Action ExampleBot ()
exampleBotBehaviour = countWords >> checkForCat
                                 >> catStats "!catStats"
                                 >> quitBot  "!die"
                                 >> schemeEval ">"
                                 >> schemeGetDefined "!defined"
                                 >> schemeClearState "!clearState" (newEnvWith 4000)

main :: IO ()
main = do
    bot <- initExampleBot
    status <- runExampleBot bot
    case status of
         Just err -> print err
         Nothing  -> putStrLn "Bye Bye!"

quitBot :: T.Text -> Action ExampleBot ()
quitBot tr = whenAdmin $ whenTrigger tr $ \dest _ -> do
    trigger <- quit <$> get
    me dest "is going offline"
    liftIO $ putMVar trigger True
    

countWords :: Action ExampleBot ()
countWords = onPrivMsg checkTrigger
    where
        checkTrigger dest (x:xs)
          | x == "?recorded" = showCountedWords dest
          | otherwise     = modify (\dat -> dat { recordedWords = recordedWords dat + (length xs) + 1})

        showCountedWords dest = do
            count <- recordedWords <$> get
            say dest $ "Recorded " `T.append` T.pack (show count) `T.append` " words."
