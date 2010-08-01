module Main (main) where

import Network.Info

main = do
    ifs <- getNetworkInterfaces
    mapM (putStrLn . showInterface) ifs

showInterface :: NetworkInterface -> String
showInterface n = netName n ++ "\n"
               ++ "IP Address:   " ++ show (netIP n) ++ "\n"
               ++ "IPv6 Address: " ++ show (netIP6 n) ++ "\n"
               ++ "MAC Address:  " ++ show (netMAC n) ++ "\n"
