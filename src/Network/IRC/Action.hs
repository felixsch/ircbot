{-# LANGUAGE OverloadedStrings #-}

module Network.IRC.Action
  ( Action(..)
  , say
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

import qualified Data.Text as T


type Destination = T.Text

say :: T.Text -> T.Text -> Action st ()
say dest msg = do
    server <- fst <$> current
    irc $ send $ mkPut server dest msg

    
me :: Destination -> T.Text -> Action st ()
me dest msg = do
    server <- fst <$> current
    irc $ send $ mkMe server dest msg

notice :: Name -> T.Text -> Action st ()
notice user msg = do
    server <- fst <$> current
    irc $ send $ mkNotice server user msg

join :: Destination -> Action st ()
join channel = do
    server <- fst <$> current
    irc $ send $ mkJoin server channel

type ActionName = T.Text

logM :: ActionName -> T.Text -> Action st ()
logM name msg = irc $ logMessage (Just name) msg


onChannel :: Channel -> ([Param] -> Action st ()) -> Action st ()
onChannel channel cmd = checkIfChannel =<< current
    where
        checkIfChannel (_, Message _ "PRIVMSG" (x:xs))
          | channel == x = cmd xs
          | otherwise    = return ()
        checkIfChannel _  = return ()

onPrivMsg :: (T.Text -> [Param] -> Action st ()) -> Action st ()
onPrivMsg action = do
    msg <- snd <$> current
    when (isCommand "PRIVMSG" msg) $ setupAction msg
    where
        setupAction (Message _ _ (x:xs)) = action x xs


whenTrigger :: T.Text -> (T.Text -> [Param] -> Action m ()) -> Action m ()
whenTrigger trigger cmd = onPrivMsg checkTrigger
    where
        checkTrigger dest (x:params)
          | x == trigger = cmd dest params
          | otherwise = return ()

isCommand :: T.Text -> Message -> Bool
isCommand cmd (Message _ cmd' _)
    | cmd == cmd' = True
    | otherwise   = False

isChannel :: T.Text -> Bool
isChannel = check . T.unpack
    where
      check ('#':_) = True
      check _       = False

type Privilige = T.Text
type Cloak     = T.Text

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

