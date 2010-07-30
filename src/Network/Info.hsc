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
import Text.Printf


-- FFI
----------------------------------------------------------------------

#include "network.h"

foreign import ccall unsafe "c_get_network_interfaces"
        c_get_network_interfaces :: Ptr NetworkInterface -> CInt -> IO CInt

#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)


-- Network interfaces
----------------------------------------------------------------------

data NetworkInterface = NetworkInterface
    { netName :: String
    , netIP :: IPv4
    , netIP6 :: IPv6
    , netMAC :: MAC
    } deriving (Show)

instance Storable NetworkInterface where
    alignment _ = #alignment struct network_interface
    sizeOf _    = #size struct network_interface
    peek ptr    = do
        name <- peekCWString $ (#ptr struct network_interface, name) ptr
        ip   <- (#peek struct network_interface, ip_address) ptr
        ip6  <- (#peek struct network_interface, ip6_address) ptr
        mac  <- (#peek struct network_interface, mac_address) ptr
        return $ NetworkInterface name ip ip6 mac


-- | Gets information about the network interfaces on the local computer
getNetworkInterfaces :: IO [NetworkInterface]
getNetworkInterfaces =
    allocaArray 64 $ \ptr -> do
    count <- c_get_network_interfaces ptr 64
    peekArray (fromIntegral count) ptr


-- IPv4 addresses
----------------------------------------------------------------------

data IPv4 = IPv4 {-# UNPACK #-} !Word32
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


-- IPv6 addresses
----------------------------------------------------------------------

data IPv6 = IPv6
    {-# UNPACK #-} !Word32
    {-# UNPACK #-} !Word32
    {-# UNPACK #-} !Word32
    {-# UNPACK #-} !Word32
    deriving (Eq, Ord, Bounded)

instance Show IPv6 where
    show = showIPv6

instance Storable IPv6 where
    alignment _ = 1
    sizeOf _    = 16
    peek p      = do
        let ptr = castPtr p
        a <- peekElemOff ptr 0
        b <- peekElemOff ptr 1
        c <- peekElemOff ptr 2
        d <- peekElemOff ptr 3
        return $ IPv6 a b c d
    poke p (IPv6 a b c d) = do
        let ptr = castPtr p
        pokeElemOff ptr 0 a
        pokeElemOff ptr 1 b
        pokeElemOff ptr 2 c
        pokeElemOff ptr 3 d


toWords :: IPv6 -> (Word32, Word32, Word32, Word32)
toWords (IPv6 a b c d) = (a, b, c, d)

fromWords :: (Word32, Word32, Word32, Word32) -> IPv6
fromWords (a, b, c, d) = IPv6 a b c d

showIPv6 :: IPv6 -> String
showIPv6 (IPv6 a b c d) = concat . intersperse "." $ concatMap showOctets [a,b,c,d]
    where showOctets x = map (show . getWord8 x) [0..3]
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
