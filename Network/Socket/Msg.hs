module Network.Socket.Msg
    ( CMsg(..)
    , CMsgable (..)
    , filterCMsgs
    , sendMsg
    , recvMsg
    , sendMMsg
    ) where

import Network.Socket.Msg.CMsg
import Network.Socket.Msg.CMsgHdr
import Network.Socket.Msg.Internal
import Network.Socket.Msg.IOVec
import Network.Socket.Msg.MsgHdr

import Control.Applicative
import Control.Monad (void)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as BU
import Network.Socket
import Network.Socket.Internal (peekSockAddr,pokeSockAddr,sizeOfSockAddr,throwSocketErrorWaitRead,throwSocketErrorWaitWrite)
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Storable (Storable(..),poke)
import Foreign.Ptr (Ptr,castPtr,plusPtr,nullPtr)

isNullPtr :: Ptr a -> Bool
isNullPtr = (==) nullPtr

{-
The buffer in both functions is filled as follows:
-------------
| Data (sz) |
-------------
|  SockAddr |
-------------
|   IOVec   |
-------------
|   MsgHdr  |
-------------
|  Aux Data |
-------------
-}

-- |Sends the data contained in the bytestring to the specified address.
-- The last argument is a list of control parameters (see cmsg(3) for details).
sendMsg :: Socket -> ByteString -> SockAddr -> [CMsg] -> IO ()
sendMsg sock@(MkSocket sockfd _ _ _ _) bytes sa cmsgs = void $ allocaBytes bufSz $ \bufPtr -> do
        let saPtr = castPtr bufPtr
        let iovPtr = plusPtr saPtr $ sizeOfSockAddr sa
        let msgPtr = plusPtr iovPtr $ sizeOf (undefined :: IOVec)
        let auxPtr = plusPtr msgPtr $ sizeOf (undefined :: MsgHdr)

        poke msgPtr MsgHdr { msgName = saPtr
                           , msgNameLen = fromIntegral $ sizeOfSockAddr sa
                           , msgIov = iovPtr
                           , msgIovLen = 1
                           , msgControl = auxPtr
                           , msgControlLen = fromIntegral auxSz
                           , msgFlags = 0 }
        pokeCMsgs msgPtr cmsgs
        pokeSockAddr saPtr sa

# if !defined(__HUGS__)
        throwSocketErrorWaitWrite sock "sendMsg" $
# endif
            BU.unsafeUseAsCStringLen bytes $ \(p,len) ->
                poke iovPtr (IOVec p $ fromIntegral len) >> c_sendmsg sockfd msgPtr 0
    where
        auxSz = sum $ map cmsgSpace cmsgs
        bufSz = sum [auxSz, sizeOfSockAddr sa, sizeOf (undefined :: MsgHdr), sizeOf (undefined :: IOVec)]

-- |Receive data and put it into a bytestring.
recvMsg :: Socket -> Int -> IO (ByteString, SockAddr, [CMsg])
recvMsg sock@(MkSocket sockfd _ _ _ _) sz = allocaBytes bufSz $ \bufPtr -> do
        let addrPtr = plusPtr bufPtr sz
        let iovPtr = plusPtr addrPtr addrSz
        let mhdrPtr = plusPtr iovPtr (sizeOf (undefined :: IOVec))
        let auxPtr = plusPtr mhdrPtr (sizeOf (undefined :: MsgHdr))

        let iov = IOVec bufPtr (fromIntegral sz)
        let msghdr = MsgHdr { msgName = addrPtr
                            , msgNameLen = fromIntegral addrSz
                            , msgIov = iovPtr
                            , msgIovLen = 1
                            , msgControl = auxPtr
                            , msgControlLen = fromIntegral auxSz
                            , msgFlags = 0 }
        poke iovPtr iov
        poke mhdrPtr msghdr

        rsz <- fmap fromIntegral (
# if !defined(__HUGS__)
            throwSocketErrorWaitRead sock "recvMsg" $
# endif
            c_recvmsg sockfd mhdrPtr 0)
        (,,) <$> B.packCStringLen (bufPtr,rsz)
             <*> peekSockAddr addrPtr
             <*> extractCMsgs mhdrPtr
    where
        -- Assume that 1024 bytes is enough for auxillary data
        auxSz = 1024
        -- Reserve 16 bytes for peer address
        addrSz = 16
        bufSz = sum [sz, auxSz, addrSz, sizeOf (undefined :: MsgHdr), sizeOf (undefined :: IOVec)]

sendMMsg :: Socket -> [(ByteString, SockAddr, [CMsg])] -> IO ()
sendMMsg sock dat = void $ allocaBytes (numMsgs * sizeOf (undefined :: MMsgHdr)) $ \mmsgBuf ->
                                                                                     pokeAndSend mmsgBuf 0 dat

  where
    numMsgs = length dat
    pokeAndSend ptr _ [] = throwSocketErrorWaitWrite sock "sendMMsg" $
                             c_sendmmsg (fdSocket sock) ptr (fromIntegral numMsgs) 0
    pokeAndSend ptr n ((bytes, sa, cmsgs):rest) = let auxSz = sum (map cmsgSpace cmsgs)
                                                      sz = auxSz + sizeOf (undefined :: IOVec) + sizeOfSockAddr sa
                                                  in allocaBytes sz $ \bufPtr ->
                                                       BU.unsafeUseAsCStringLen bytes $ \(datPtr, datLen) -> do
                                                         let iovPtr = castPtr bufPtr
                                                             saPtr = iovPtr `plusPtr` sizeOf (undefined :: IOVec)
                                                             auxPtr = saPtr `plusPtr` sizeOfSockAddr sa
                                                             mmsgPtr = ptr `plusPtr` (n * sizeOf (undefined :: MMsgHdr))
                                                         pokeSockAddr saPtr sa
                                                         poke iovPtr (IOVec datPtr $ fromIntegral datLen)
                                                         poke mmsgPtr $ MMsgHdr (MsgHdr { msgName = saPtr
                                                                                        , msgNameLen = fromIntegral $ sizeOfSockAddr sa
                                                                                        , msgIov = iovPtr
                                                                                        , msgIovLen = 1
                                                                                        , msgControl = auxPtr
                                                                                        , msgControlLen = fromIntegral auxSz
                                                                                        , msgFlags = 0
                                                                                        }) 0
                                                         pokeCMsgs (castPtr mmsgPtr) cmsgs
                                                         pokeAndSend ptr (n + 1) rest



extractCMsgs :: Ptr MsgHdr -> IO [CMsg]
extractCMsgs pMsg = extractCMsgs' (c_cmsg_firsthdr pMsg) []
    where
        extractCMsgs' pCMsg resList
            | isNullPtr pCMsg = return resList
            | otherwise = do
                mcmsg <- peekCMsg pCMsg
                extractCMsgs' (c_cmsg_nexthdr pMsg pCMsg)
                              (case mcmsg of
                                  Nothing -> resList
                                  Just cmsg -> cmsg:resList)
