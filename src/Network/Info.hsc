{-# LANGUAGE ForeignFunctionInterface #-}

module Network.Info (
    getNetworkInterfaces,
    NetworkInterface (..),
    MAC (..),
) where

import Data.Bits ((.&.), shiftR, shiftL)
import Data.List (intersperse)
import Data.Word
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable
import Numeric (showHex)
import Text.Printf
import Network.Socket
import Network.Socket.Internal


----------------------------------------------------------------------
-- FFI
----------------------------------------------------------------------

#include "network.h"
#include "list.h"

foreign import ccall unsafe "c_get_network_interfaces"
        c_get_network_interfaces :: Ptr NetworkInterface -> CInt -> IO CInt

foreign import ccall unsafe "list_free"
  listFree :: Ptr () -> IO ()

#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)


----------------------------------------------------------------------
-- Network interfaces
----------------------------------------------------------------------

-- | Describes the basic configuration of a network interface. /This/
--   /definition is currently limited to just one address per family./
data NetworkInterface = NetworkInterface
    { name :: String -- ^ Interface name (e.g. \"eth0\", \"lo\", \"Local Area Connection\")
    , addresses :: [SockAddr]
    , mac  :: MAC    -- ^ MAC address
    } deriving (Show)

instance Storable NetworkInterface where
    alignment _ = #alignment struct network_interface
    sizeOf _    = #size struct network_interface
    peek ptr    = do
        name <- peekCWString $ (#ptr struct network_interface, name) ptr
        addrListPtr <- (#peek struct network_interface, addresses) ptr
        addrs <- crawlAddrList addrListPtr
        mac  <- (#peek struct network_interface, mac_address) ptr
        listFree addrListPtr
        return $ NetworkInterface name addrs mac

crawlAddrList :: Ptr () -> IO [SockAddr]
crawlAddrList ptr | ptr == nullPtr = return []
                  | otherwise = do
    addr <- peekSockAddr =<< (#peek struct addr_list, payload) ptr
    next <- (#peek struct addr_list, next) ptr
    (addr :) `fmap` (crawlAddrList next)




-- | Gets the address information for each of the network interfaces on
--   the local computer.
getNetworkInterfaces :: IO [NetworkInterface]
getNetworkInterfaces =
    allocaArray 64 $ \ptr -> do
    count <- c_get_network_interfaces ptr 64
    peekArray (fromIntegral count) ptr

----------------------------------------------------------------------
-- MAC addresses
----------------------------------------------------------------------

-- | Represents a MAC address (e.g. @01:23:45:67:89:ab@)
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
