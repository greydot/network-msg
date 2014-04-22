{-# LANGUAGE ForeignFunctionInterface #-}

module Network.Socket.Msg.CMsg
    ( CSockLen
    , CMsg(..)
    , CMsgHdr(..)
    , c_cmsg_firsthdr
    , c_cmsg_nexthdr
    , c_cmsg_data
    , peekCMsg
    , pokeCMsg
    ) where

#include <sys/types.h>
#include <sys/socket.h>

import Network.Socket.Msg.MsgHdr (MsgHdr)

import qualified Data.ByteString as B
import Data.Maybe (isNothing,fromJust)
import Foreign.C.Types (CUInt(..),CInt(..))
import Foreign.Marshal.Utils (copyBytes)
import Foreign.Ptr (Ptr,castPtr,nullPtr)
import Foreign.Storable (Storable(..))

type CSockLen = CUInt   -- The way it is defined somewhere in bits/types.h

data CMsg = CMsg
    { cmsgLevel :: Int
    , cmsgType  :: Int
    , cmsgData  :: B.ByteString
    }

data CMsgHdr = CMsgHdr
    { cmsgLen       :: CSockLen
    , cmsghdrLevel  :: CInt
    , cmsghdrType   :: CInt
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
        (#poke struct cmsghdr, cmsg_level) p (cmsghdrLevel cmh)
        (#poke struct cmsghdr, cmsg_type) p (cmsghdrType cmh)

-- The manual says the following functions (actually macros)
-- are constant and thus we do not have to use IO monad.

foreign import ccall unsafe "cmsg_firsthdr"
  c_cmsg_firsthdr :: Ptr MsgHdr -> Ptr CMsgHdr

foreign import ccall unsafe "cmsg_nexthdr"
  c_cmsg_nexthdr :: Ptr MsgHdr -> Ptr CMsgHdr -> Ptr CMsgHdr

foreign import ccall unsafe "cmsg_data"
  c_cmsg_data :: Ptr CMsgHdr -> Ptr ()

cmsgExtractData :: Ptr CMsgHdr -> IO (Maybe B.ByteString)
cmsgExtractData p = do
    let dataPtr = castPtr $ c_cmsg_data p
    dataLen <- return . cmsgLen =<< peek p
    if dataPtr == nullPtr
        then return Nothing
        else return.Just =<< B.packCStringLen (dataPtr, fromIntegral dataLen)

peekCMsg :: Ptr CMsgHdr -> IO (Maybe CMsg)
peekCMsg pCMsgHdr =
        peek pCMsgHdr >>= \cmsghdr ->
        cmsgExtractData pCMsgHdr >>= \dat ->
        return $ if isNothing dat
            then Nothing
            else Just CMsg { cmsgLevel = fromIntegral $ cmsghdrLevel cmsghdr
                           , cmsgType = fromIntegral $ cmsghdrType cmsghdr
                           , cmsgData = fromJust dat }

pokeCMsg :: Ptr CMsgHdr -> CMsg -> IO ()
pokeCMsg pHdr cmsg = do
        poke pHdr cmsghdr
        let dptr = castPtr $ c_cmsg_data pHdr
        B.useAsCStringLen (cmsgData cmsg) $ \(bptr,len) -> copyBytes dptr bptr len
    where
        cmsghdr = CMsgHdr { cmsgLen = fromIntegral $ B.length $ cmsgData cmsg
                          , cmsghdrLevel = fromIntegral $ cmsgLevel cmsg
                          , cmsghdrType = fromIntegral $ cmsgType cmsg }
