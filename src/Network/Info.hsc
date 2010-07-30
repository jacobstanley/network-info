{-# LANGUAGE ForeignFunctionInterface #-}

module Network.Info (
    getNetworkInterfaces,
    NetworkInterface (..),
    MAC,
) where

import Data.Word
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable
import Text.Printf


#include "network.h"

foreign import ccall unsafe "c_get_network_interfaces"
        c_get_network_interfaces :: Ptr NetworkInterface -> CInt -> IO CInt


data NetworkInterface = NetworkInterface
    { netName :: String
    , netIP :: String
    , netBroadcast :: String
    , netMAC :: MAC
    } deriving (Show)


-- | Gets information about the network interfaces on the local computer
getNetworkInterfaces :: IO [NetworkInterface]
getNetworkInterfaces =
    allocaArray 64 $ \ptr -> do
    count <- c_get_network_interfaces ptr 64
    peekArray (fromIntegral count) ptr


#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)

instance Storable NetworkInterface where
    alignment _ = #alignment struct network_interface
    sizeOf _    = #size struct network_interface
    peek ptr    = do
        name  <- peekCString $ (#ptr struct network_interface, name) ptr
        inet  <- peekCString $ (#ptr struct network_interface, inet) ptr
        bcast <- peekCString $ (#ptr struct network_interface, bcast) ptr
        mac   <- (#peek struct network_interface, mac) ptr
        return $ NetworkInterface name inet bcast mac


data MAC = MAC
    {-# UNPACK #-} !Word8
    {-# UNPACK #-} !Word8
    {-# UNPACK #-} !Word8
    {-# UNPACK #-} !Word8
    {-# UNPACK #-} !Word8
    {-# UNPACK #-} !Word8
    deriving (Eq, Ord, Bounded)

instance Show MAC where
    show (MAC a b c d e f) = printf "%02x:%02x:%02x:%02x:%02x:%02x" a b c d e f

instance Storable MAC where
    alignment _ = 1
    sizeOf _    = 6
    peek p      = do
        a <- peek $ castPtr p
        b <- peekByteOff p 1
        c <- peekByteOff p 2
        d <- peekByteOff p 3
        e <- peekByteOff p 4
        f <- peekByteOff p 5
        return $ MAC a b c d e f
    poke p (MAC a b c d e f) = do
        poke (castPtr p) a
        pokeByteOff p 1 b
        pokeByteOff p 2 c
        pokeByteOff p 3 d
        pokeByteOff p 4 e
        pokeByteOff p 5 f
