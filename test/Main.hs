module Main (main) where

import Network.Info

main = do
    ifcs <- getNetworkInterfaces
    mapM print ifcs
