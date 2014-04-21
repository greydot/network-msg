module Network.Socket.Msg.CMsg
    ( CSockLen
    , CMsgHdr(..)
    ) where

#include <sys/types.h>
#include <sys/socket.h>

import Foreign.C.Types (CUInt,CInt)
import Foreign.Storable (Storable(..))

type CSockLen = CUInt   -- The way it is defined somewhere in bits/types.h

data CMsgHdr = CMsgHdr
    { cmsgLen   :: CSockLen
    , cmsgLevel :: CInt
    , cmsgType  :: CInt
    }

instance Storable CMsgHdr where
    sizeOf _ = (#const sizeof(struct cmsghdr))
    alignment _ = alignment (undefined :: CInt)

    peek p = do
        len <- (#peek struct cmsghdr, cmsg_len) p
        level <- (#peek struct cmsghdr, cmsg_level) p
        t <- (#peek struct cmsghdr, cmsg_type) p
        return $ CMsgHdr len level t

    poke p cmh = do
        (#poke struct cmsghdr, cmsg_len) p (cmsgLen cmh)
        (#poke struct cmsghdr, cmsg_level) p (cmsgLevel cmh)
        (#poke struct cmsghdr, cmsg_type) p (cmsgType cmh)
