module Main (main) where

import Network.Info

main = do
    ns <- getNetworkInterfaces
    mapM (putStrLn . showInterface) ns

showInterface :: NetworkInterface -> String
showInterface n = name n ++ "\n"
               ++ "Addresses: " ++ show (addresses n) ++ "\n"
               ++ "MAC Address:  " ++ show (mac n) ++ "\n"
