module Main (main) where

import Data.List (intercalate)
import Network (withSocketsDo)
import Network.Info

main = withSocketsDo $ do
    ns <- getNetworkInterfaces
    mapM (putStrLn . showInterface) ns

showInterface :: NetworkInterface -> String
showInterface n = name n ++ "\n"
               ++ "Addresses: " ++ intercalate ", " addrs ++ "\n"
               ++ "MAC Address:  " ++ show (mac n) ++ "\n"
  where
    addrs = map show (addresses n)
