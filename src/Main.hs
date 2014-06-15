{-# LANGUAGE OverloadedStrings #-}
import Control.Monad.IO.Class


import Network.IRC
import Network.IRC.Conduit
import Network.IRC.Command
import Network.IRC.Message

import Data.Conduit
import qualified Data.Conduit.List as CL
import Data.Text hiding (last)

main :: IO ()
main = runIRC testClient (onPing =$= onChannel "#felixsch" onFelixsch =$= logIRC)

logIRC :: (MonadIO m) => IRCConduit m
logIRC = awaitForever handleIRC
    where
      handleIRC s@(Left msg) = (liftIO $ print msg)
      handleIRC s@(Right cmd) = (liftIO $ putStrLn $ "> " ++ show cmd) >> yield s


onPing :: (MonadIO m) => IRCConduit m
onPing = onAction "PING" $ awaitForever pong
    where
     pong (Left (Message _ _ (x:xs))) = send $ raw "PONG" [':' `cons` x] Nothing
     pong x                           = yield x


onFelixsch :: (MonadIO m) => IRCConduit m
onFelixsch = do
    msg <- await
    case msg of
        Just (Left (Message _ _ pa)) -> send $ raw "PRIVMSG" ["#felixsch"] (Just $ "echo: " `append` getMsg pa)
        otherwise                    -> return ()
    where
      getMsg [] = empty
      getMsg x  = last x
