{-# LANGUAGE OverloadedStrings #-}
module Module.CatDetection
    ( CatEnv(..)
    , newCatEnv
    , WithCatDetection(..)
    , catStats
    , checkForCat
    ) where

import Control.Monad
import Control.Applicative

import Control.Monad.IO.Class

import qualified Data.Text as T

import Network.URI
import Network.HTTP.Conduit     ( simpleHttp )

import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as BL

import Foreign.C.Types
import Foreign.C.String

import Network.IRC.Action


foreign import ccall "detectTheCat"
    c_detectTheCat :: CString -> CString -> CFloat -> CInt -> CInt

detectCat :: FilePath -> FilePath -> Float -> Int -> IO Bool
detectCat img mdl thre threads = do
    image <- newCString img
    model <- newCString mdl
    let ret = c_detectTheCat image model (realToFrac thre) (toEnum threads)
    return $ ret > 0

data CatEnv st = CatEnv { catImages   :: [(T.Text, Bool)] }


class WithCatDetection st where
    catTmpImage   :: Action st FilePath
    catModel      :: Action st FilePath
    catFoundMsg   :: Action st T.Text
    catRememberMsg :: Action st T.Text

    catGetEnv     :: Action st (CatEnv st)
    catSetEnv     :: CatEnv st -> Action st ()

    catModEnv     :: (CatEnv st -> CatEnv st) -> Action st ()
    catModEnv f = do
        env <- catGetEnv
        catSetEnv (f env) 

newCatEnv :: (WithCatDetection st) => CatEnv st
newCatEnv = CatEnv []

catStats :: (WithCatDetection st) => T.Text -> Action st ()
catStats tr = whenTrigger tr $ \dest _ -> do
    dat <- catGetEnv
    say dest $ "cats found : " `T.append` T.pack (show $ countKittens $ catImages dat)
    say dest $ "total images  : " `T.append` T.pack (show $ Prelude.length $ catImages dat)
    say dest   "last 3 images : "
    forM_ (Prelude.take 3 $ catImages dat) $ \(url,isCat) ->
        say dest $ if isCat 
            then "  - " `T.append` url `T.append` " (detected as cat)"
            else "  - " `T.append` url
    where
        countKittens :: [(T.Text, Bool)] -> Int
        countKittens ((_,True):xs) = 1 + countKittens xs
        countKittens (_:xs)        = 0 + countKittens xs
        countKittens []            = 0


checkForCat :: (WithCatDetection st) => Action st ()
checkForCat = onPrivMsg $ \dest txt ->
    forM_ (concatMap T.words txt) $ \word -> whenURI word $
        case isSupported word of
            Just ty  -> checkImage dest word ty
            Nothing  -> return ()

whenURI :: (MonadIO m) => T.Text -> m () -> m ()
whenURI text = when (isURI $ T.unpack text)


checkImage :: (WithCatDetection st) => T.Text -> T.Text -> String -> Action st ()
checkImage dest url ty = do
    imgs     <- catImages <$> catGetEnv
    remember <- catRememberMsg
    found    <- catFoundMsg
    model    <- catModel
    tmpImage <- catTmpImage

    case lookup url imgs of
        Just isCat -> when isCat $ say dest remember
        Nothing    -> do
            image <- liftIO $ simpleHttp $ T.unpack url
            liftIO $ BL.writeFile (tmp tmpImage) image
 
            isCat <- liftIO $ detectCat (tmp tmpImage) model (-0.4) 4

            catModEnv(\dat -> dat { catImages = (url,isCat) : catImages dat })
 
            when isCat $ say dest found
    where
        tmp t = t ++ "." ++ ty


isSupported :: T.Text -> Maybe String
isSupported = isSup . reverse . T.unpack
    where
        isSup ('g':'p':'j':_) = Just "jpg"
        isSup ('g':'n':'p':_) = Just "png"
        isSup ('g':'e':'p':'j':_) = Just "jpeg"
        isSup _               = Nothing

