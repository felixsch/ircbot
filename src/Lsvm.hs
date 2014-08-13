module Lsvm
    ( detectCat
    ) where

import Foreign.C.Types
import Foreign.C.String

foreign import ccall "detectTheCat"
    c_detectTheCat :: CString -> CString -> CFloat -> CInt -> CInt



detectCat :: FilePath -> FilePath -> Float -> Int -> IO Bool
detectCat img mdl thre threads = do
    image <- newCString img
    model <- newCString mdl
    let ret = c_detectTheCat image model (realToFrac thre) (toEnum threads)
    return $ ret > 0

