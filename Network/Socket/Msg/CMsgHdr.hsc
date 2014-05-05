{-# LANGUAGE ForeignFunctionInterface #-}

module Network.Socket.Msg.CMsgHdr
    ( CSockLen
    , CMsg(..)
    , CMsgHdr(..)
    , c_cmsg_firsthdr
    , c_cmsg_nexthdr
    , c_cmsg_data
    , cmsgSpace
    , peekCMsg
    , pokeCMsg
    , pokeCMsgs
    ) where

#include <sys/types.h>
#include <sys/socket.h>

import Network.Socket.Msg.CMsg (CMsg(..))
import Network.Socket.Msg.MsgHdr (MsgHdr(..))

import Control.Applicative
import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as BU
import Data.Maybe (isNothing,fromJust)
import Foreign.C.Types (CUInt(..),CInt(..),CSize(..))
import Foreign.Marshal.Unsafe (unsafeLocalState)
import Foreign.Marshal.Utils (copyBytes)
import Foreign.Ptr (Ptr,castPtr,nullPtr)
import Foreign.Storable (Storable(..))

type CSockLen = CUInt   -- The way it is defined somewhere in bits/types.h

data CMsgHdr = CMsgHdr
    { cmsghdrLen       :: !CSockLen
    , cmsghdrLevel  :: !CInt
    , cmsghdrType   :: !CInt
    } deriving (Show)

instance Storable CMsgHdr where
    sizeOf _ = (#const sizeof(struct cmsghdr))
    alignment _ = alignment (undefined :: CInt)

    -- XXX: The following ifdefs are needed because on Linux
    -- cmsg_len has type size_t which is twice as large as
    -- socklen_t.
    -- And as always I thank linux developers for both excellent
    -- documentation and standard compliance. Motherfuckers.

    peek p = do
# if linux_HOST_OS == 1
        len <- fmap fromIntegral
            ( ((#peek struct cmsghdr, cmsg_len) $ castPtr p) :: IO CSize )
# else
        len <- (#peek struct cmsghdr, cmsg_len) p
# endif
        level <- (#peek struct cmsghdr, cmsg_level) p
        t <- (#peek struct cmsghdr, cmsg_type) p
        return $! CMsgHdr len level t

    poke p cmh = do
# if linux_HOST_OS == 1
        (#poke struct cmsghdr, cmsg_len) (castPtr p) (fromIntegral $ cmsghdrLen cmh :: CSize)
# else
        (#poke struct cmsghdr, cmsg_len) p (cmsghdrLen cmh)
# endif
        (#poke struct cmsghdr, cmsg_level) p (cmsghdrLevel cmh)
        (#poke struct cmsghdr, cmsg_type) p (cmsghdrType cmh)

-- The manual says the following functions (actually macros)
-- are constant and thus we do not have to use IO monad.

foreign import ccall unsafe "cmsg_firsthdr"
  _c_cmsg_firsthdr :: Ptr MsgHdr -> IO (Ptr CMsgHdr)

foreign import ccall unsafe "cmsg_nexthdr"
  _c_cmsg_nexthdr :: Ptr MsgHdr -> Ptr CMsgHdr -> IO (Ptr CMsgHdr)

foreign import ccall unsafe "cmsg_data"
  _c_cmsg_data :: Ptr CMsgHdr -> IO (Ptr ())

c_cmsg_firsthdr :: Ptr MsgHdr -> Ptr CMsgHdr
c_cmsg_firsthdr = unsafeLocalState . _c_cmsg_firsthdr

c_cmsg_nexthdr :: Ptr MsgHdr -> Ptr CMsgHdr -> Ptr CMsgHdr
c_cmsg_nexthdr pm = unsafeLocalState . _c_cmsg_nexthdr pm

c_cmsg_data :: Ptr CMsgHdr -> Ptr ()
c_cmsg_data = unsafeLocalState . _c_cmsg_data

foreign import ccall unsafe "cmsg_space"
  c_cmsg_space :: CSize -> CSize

foreign import ccall unsafe "cmsg_len"
  c_cmsg_len :: CSize -> CSize

cmsgSpace :: CMsg -> Int
cmsgSpace = spc . B.length . cmsgData
    where spc = fromIntegral . c_cmsg_space . fromIntegral

cmsgLen :: CMsg -> Int
cmsgLen = len . B.length . cmsgData
    where len = fromIntegral . c_cmsg_len . fromIntegral

cmsgExtractData :: Ptr CMsgHdr -> IO (Maybe B.ByteString)
cmsgExtractData p = do
    let dataPtr = castPtr $ c_cmsg_data p
    dataLen <- cmsghdrLen <$> peek p
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
        BU.unsafeUseAsCStringLen (cmsgData cmsg) $ \(bptr,len) -> copyBytes dptr bptr len
    where
        cmsghdr = CMsgHdr { cmsghdrLen = fromIntegral $ cmsgLen cmsg
                          , cmsghdrLevel = fromIntegral $ cmsgLevel cmsg
                          , cmsghdrType = fromIntegral $ cmsgType cmsg }

pokeCMsgs :: Ptr MsgHdr -> [CMsg] -> IO ()
pokeCMsgs pMsg cmsgs = do
        msg <- peek pMsg
        cLen <- pokeCMsgs' (c_cmsg_firsthdr pMsg) cmsgs
        poke pMsg $ msg { msgControlLen = fromIntegral cLen }
    where
        pokeCMsgs' :: Ptr CMsgHdr -> [CMsg] -> IO Int
        pokeCMsgs' _ [] = return 0
        pokeCMsgs' pCMsg (c:cs) = do
                pokeCMsg pCMsg c
                ((+)$cmsgSpace c) <$> pokeCMsgs' (c_cmsg_nexthdr pMsg pCMsg) cs
