{-# LANGUAGE ForeignFunctionInterface #-}

module Network.Info (
    getNetworkInterfaces,
    NetworkInterface (..),
    MAC,
) where

import Data.Bits ((.&.), shiftR)
import Data.List (intersperse)
import Data.Word
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable
import Network.Socket
import Text.Printf


-- FFI
----------------------------------------------------------------------

#include "network.h"

type SocketFD = CInt

foreign import ccall unsafe "c_get_network_interfaces"
        c_get_network_interfaces :: SocketFD -> Ptr NetworkInterface -> CInt -> IO CInt

#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)


-- Network interfaces
----------------------------------------------------------------------

data NetworkInterface = NetworkInterface
    { netName :: String
    , netAddress :: IPv4
    , netMask :: IPv4
    , netMAC :: MAC
    } deriving (Show)

instance Storable NetworkInterface where
    alignment _ = #alignment struct network_interface
    sizeOf _    = #size struct network_interface
    peek ptr    = do
        name  <- peekCString $ (#ptr struct network_interface, name) ptr
        inet  <- (#peek struct network_interface, address) ptr
        mask  <- (#peek struct network_interface, netmask) ptr
        mac   <- (#peek struct network_interface, mac_address) ptr
        return $ NetworkInterface name inet mask mac


-- | Gets information about the network interfaces on the local computer
getNetworkInterfaces :: IO [NetworkInterface]
getNetworkInterfaces = withSocketFD $ \fd -> do
    allocaArray 64 $ \ptr -> do
    count <- c_get_network_interfaces fd ptr 64
    peekArray (fromIntegral count) ptr


withSocketFD :: (SocketFD -> IO a) -> IO a
withSocketFD f = do
    s <- socket AF_INET Datagram 0
    res <- f (fdSocket s)
    sClose s
    return res


-- IPv4 addresses
----------------------------------------------------------------------

newtype IPv4 = IPv4 Word32
    deriving (Eq, Ord, Bounded)

instance Show IPv4 where
    show = showIPv4

instance Storable IPv4 where
    alignment _ = 1
    sizeOf _    = 4
    peek p      = peek (castPtr p) >>= \ip -> return (IPv4 ip)
    poke p (IPv4 ip) = poke (castPtr p) ip


toWord32 :: IPv4 -> Word32
toWord32 (IPv4 ip) = ip

fromWord32 :: Word32 -> IPv4
fromWord32 = IPv4

showIPv4 :: IPv4 -> String
showIPv4 (IPv4 ip) = concat . intersperse "." $ showOctets
    where showOctets = map (show . getWord8 ip) [0..3]
          getWord8 :: Word32 -> Int -> Word8
          getWord8 w32 n = fromIntegral $ w32 `shiftR` (n * 8) .&. 0xff


-- MAC addresses
----------------------------------------------------------------------

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
