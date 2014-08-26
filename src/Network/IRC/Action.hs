{-# LANGUAGE OverloadedStrings #-}

module Network.IRC.Action
  ( say
  , me
  , notice
  , join
  , logM
  , onChannel
  , onPrivMsg
  , whenTrigger
  , Cloak
  , genCloak
  , Privilige
  , WithPriviliges(..)
  , withPriviliges
  , whenAdmin
  , whenMod

  )
  where

import Control.Applicative
import Control.Monad        ( when )

import Network.IRC
import Network.IRC.Command
import Network.IRC.Message
import qualified Data.ByteString.Char8 as B

type Destination = B.ByteString

say :: B.ByteString -> B.ByteString -> Action st ()
say dest msg = do
    server <- fst <$> current
    irc $ send $ mkPut server dest msg

    
me :: Destination -> B.ByteString -> Action st ()
me dest msg = do
    server <- fst <$> current
    irc $ send $ mkMe server dest msg

notice :: Name -> B.ByteString -> Action st ()
notice user msg = do
    server <- fst <$> current
    irc $ send $ mkNotice server user msg

join :: Destination -> Action st ()
join channel = do
    server <- fst <$> current
    irc $ send $ mkJoin server channel

type ActionName = B.ByteString

logM :: ActionName -> B.ByteString -> Action st ()
logM name msg = irc $ logMessage (Just name) msg


onChannel :: Channel -> ([Param] -> Action st ()) -> Action st ()
onChannel channel cmd = checkIfChannel =<< current
    where
        checkIfChannel (_, Message _ "PRIVMSG" (x:xs))
          | channel == x = cmd xs
          | otherwise    = return ()
        checkIfChannel _  = return ()

onPrivMsg :: (B.ByteString -> [Param] -> Action st ()) -> Action st ()
onPrivMsg action = do
    msg <- snd <$> current
    when (isCommand "PRIVMSG" msg) $ setupAction msg
    where
        setupAction (Message _ _ (x:xs)) = action x xs


whenTrigger :: B.ByteString -> (B.ByteString -> [Param] -> Action m ()) -> Action m ()
whenTrigger trigger cmd = onPrivMsg checkTrigger
    where
        checkTrigger dest (x:params)
          | x == trigger = cmd dest params
          | otherwise = return ()

isCommand :: B.ByteString -> Message -> Bool
isCommand cmd (Message _ cmd' _)
    | cmd == cmd' = True
    | otherwise   = False

isChannel :: B.ByteString -> Bool
isChannel = check . B.unpack
    where
      check ('#':_) = True
      check _       = False

type Privilige = B.ByteString
type Cloak     = B.ByteString

genCloak :: Maybe Origin -> Maybe Cloak
genCloak (Just (Nickname _ (Just h))) = Just h
genCloak _                            = Nothing

class WithPriviliges st where
    isAdmin :: Cloak -> Action st Bool
    isAdmin = hasPrivilige "admin"

    isMod   :: Cloak -> Action st Bool
    isMod   = hasPrivilige "mod"

    hasPrivilige :: Privilige -> Cloak -> Action st Bool


withPriviliges :: (WithPriviliges st) => (Cloak -> Action st Bool) -> Action st () -> Action st ()
withPriviliges check action = do
    cloak <- genCloak . msgOrigin . snd <$> current
    case cloak of 
      Nothing -> return ()
      Just cl -> do
        isAllowed <- check cl
        when isAllowed action

whenAdmin :: (WithPriviliges st) => Action st () -> Action st ()
whenAdmin = withPriviliges isAdmin

whenMod :: (WithPriviliges st) => Action st () -> Action st ()
whenMod = withPriviliges isMod

