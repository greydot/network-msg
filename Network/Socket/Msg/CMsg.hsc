{-# LANGUAGE CPP,ScopedTypeVariables #-}
module Network.Socket.Msg.CMsg
    ( CMsg(..)
    , CMsgable(..)
    , filterCMsgs
#ifdef IP_PKTINFO
    , IpPktInfo(..)
#endif
    , flag_MSG_DONTWAIT
    ) where

#include <sys/socket.h>
#include <netinet/in.h>

import Control.Applicative
import Data.Binary
import Data.Binary.Get (getWord32host)
import Data.Binary.Put (putWord32host)
import qualified Data.ByteString as B
import Data.ByteString.Lazy (fromStrict,toStrict)
import Foreign.C.Types (CInt)
import Network.Socket (HostAddress)

data CMsg = CMsg
    { cmsgLevel :: !Int
    , cmsgType  :: !Int
    , cmsgData  :: !B.ByteString
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
    { ipi_ifindex   :: !Word32
    , ipi_spec_dst  :: !HostAddress
    , ipi_addr      :: !HostAddress
    } deriving (Show)

instance Binary IpPktInfo where
    put i = do
        -- XXX: Assume that sizeof(int) == 4
        putWord32host $ ipi_ifindex i
        putWord32host $ ipi_spec_dst i
        putWord32host $ ipi_addr i
    get = IpPktInfo <$> getWord32host
                    <*> getWord32host
                    <*> getWord32host

instance CMsgable IpPktInfo where
    getCMsgLevel    _ = #const IPPROTO_IP
    getCMsgType     _ = #const IP_PKTINFO

# endif

flag_MSG_DONTWAIT :: CInt
flag_MSG_DONTWAIT = #const MSG_DONTWAIT
