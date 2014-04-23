{-# LANGUAGE CPP,ScopedTypeVariables #-}
module Network.Socket.Msg.CMsg
    ( CMsg(..)
    , CMsgable(..)
    , filterCMsgs
#ifdef IP_PKTINFO
    , IpPktInfo(..)
#endif
    ) where

#include <sys/socket.h>
#include <netinet/in.h>

import Control.Applicative
import Data.Binary
import qualified Data.ByteString as B
import Data.ByteString.Lazy (fromStrict,toStrict)
import Network.Socket (HostAddress)

data CMsg = CMsg
    { cmsgLevel :: Int
    , cmsgType  :: Int
    , cmsgData  :: B.ByteString
    }

instance Show CMsg where
    show cmsg = concat ["(",
                        "Level: ", show $ cmsgLevel cmsg, ", ",
                        "Type: ", show $ cmsgType cmsg, ", ",
                        "Data: ", show $ cmsgData cmsg, ")"]

-- |Class for binary structures that can be used as control messages (cmsg(3)).
-- 
-- Complete definition requires for a type to be an instance of Binary class,
-- as well as to provide getCMsgLevel and getCMsgType methods.
--
-- Note that the argument of getCMsgLevel and getCMsgType methods should not
-- be used as it might be undefined.
class Binary a => CMsgable a where
    getCMsgLevel    :: a -> Int
    getCMsgType     :: a -> Int

    toCMsg :: a -> CMsg
    toCMsg x = CMsg { cmsgLevel = getCMsgLevel x
                    , cmsgType = getCMsgType x
                    , cmsgData = toStrict $ encode x }

    -- XXX: Find a way to check type and level values in here
    fromCMsg :: CMsg -> Maybe a
    fromCMsg cmsg = case decodeOrFail (fromStrict $ cmsgData cmsg) of
                        Left _ -> Nothing
                        Right (_,_,x) -> Just x

-- |Filter specific kind of control messages.
-- 
-- Example: filterCMsgs (undefined :: IpPktInfo) cmsgs
filterCMsgs :: (CMsgable a) => a -> [CMsg] -> [CMsg]
filterCMsgs x = filter $ \c -> (cmsgType c == getCMsgType x) && (cmsgLevel c == getCMsgLevel x)

#ifdef IP_PKTINFO
data IpPktInfo = IpPktInfo
    { ipi_ifindex   :: Int
    , ipi_spec_dst  :: HostAddress
    , ipi_addr      :: HostAddress
    }

instance Binary IpPktInfo where
    put i = do
        -- XXX: Assume that sizeof(int) == 4
        put $ ((fromIntegral $ ipi_ifindex i) :: Word32)
        put $ ipi_spec_dst i
        put $ ipi_addr i
    get = IpPktInfo <$> fmap fromIntegral (get :: Get Word32)
                    <*> get
                    <*> get

instance CMsgable IpPktInfo where
    getCMsgLevel    _ = #const IPPROTO_IP
    getCMsgType     _ = #const IP_PKTINFO

# endif
