#A simple ircbot
Because everybody nowdays seems to run a IRC bot I tought why not write and run a IRC bot too. 
I started searching for usable librayies. But on a closer look most of them doesn't support a generic way using own data structures or bound to IO.

I decided to take this chance to write my own implementation, because reinventing the wheel sometimes isn't bad!

## Features of the bot:
- Monadic way of writing behaviour
- You can use your own data structures
- Fully threaded
- Able to handle multiple connections
- Two example modules (A Scheme-like interpreter and a cat recoginition module which detect cats in pictures)

## Build & Run
>The cat recoginition module requires [OpenCV](http://opencv.org). You need to install the _development_ package first.

A `cabal` file is included. Just create a sandbox and build the bot!

    cabal sandbox init
    cabal install --dependencies-only
    cabal build

Run as:
    
    cabal run
    
> Be aware that this software is a hobby project. Therefore it is potentially full of bugs. You should use it in a save environment only (chrooted environment). Threat the cat detection module with extra caution!

# Define your own action
Defining your own action is easy.
There are two helper functions:
```
-- Specific action in a channel
onChannel :: Channel -> ([Param] -> Action st ()) -> Action st ()

-- On any privmsg
onPrivMsg :: (Text -> [Param] -> Action st ()) -> Action st ()

-- On a prefix (eg. "!action")
onTrigger :: Text -> (Text -> [Param] -> Action st ()) -> Action st ()
```
With these three function most action coudl be implemented

```haskell
 {-# LANGUAGE OverloadedStrings #-}
 
 import qualified Data.Text as T
 
 myOwnAction :: Action st ()
 myOwnAction = onTrigger "!dosomething" $ \destination params ->
   say destination $ "You said: " ++ T.unwords params
```
    


    

