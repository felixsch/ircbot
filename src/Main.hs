import System.IO


import Network.IRC.Conduit
import Network.IRC.Client


main :: IO ()
main = runIRC testClient testCon
