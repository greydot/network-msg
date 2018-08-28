module Network.Socket.Msg
    ( CMsg(..)
    , CMsgable (..)
    , filterCMsgs
    , sendMsg
    , recvMsg
    , sendMMsg
    , recvMMsg
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
import Data.Foldable (for_)
import Data.Traversable (for)
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

recvMMsg :: Socket -> Int -> Int -> IO [(ByteString, SockAddr, [CMsg])]
recvMMsg sock maxNum maxSz = allocaBytes bufSz $ \bufPtr -> do
                               for_ [0..maxNum-1] $ \n ->
                                 let iovp = iovPtr n $ castPtr bufPtr
                                     mmsg = MMsgHdr (MsgHdr { msgName = addrPtr n $ castPtr bufPtr
                                                            , msgNameLen = fromIntegral addrSz
                                                            , msgIov = iovp
                                                            , msgIovLen = 1
                                                            , msgControl = auxPtr n $ castPtr bufPtr
                                                            , msgControlLen = fromIntegral auxSz
                                                            , msgFlags = 0
                                                            }) 0
                                     iov = IOVec (datPtr n $ castPtr bufPtr) $ fromIntegral maxSz
                                 in poke iovp iov >> poke (mmsgPtr n bufPtr) mmsg
                               retNum <- throwSocketErrorWaitRead sock "recvMMsg" $
                                 c_recvmmsg (fdSocket sock) bufPtr (fromIntegral maxNum) flag_MSG_DONTWAIT nullPtr
                               for [0..fromIntegral retNum - 1] $ \n -> do
                                 let mmsgp = mmsgPtr n bufPtr
                                 MMsgHdr msghdr datLen <- peek mmsgp
                                 IOVec dat _ <- peek (msgIov msghdr)
                                 cmsgs <- extractCMsgs (castPtr mmsgp)
                                 bs <- B.packCStringLen (dat, fromIntegral datLen)
                                 sa <- peekSockAddr (msgName msghdr)
                                 pure (bs, sa, cmsgs)
  where
    auxSz = 1024
    addrSz = 16  -- Reserve 16 bytes for peer address
    bufSz = maxNum * sum [ sizeOf (undefined :: MMsgHdr)
                         , sizeOf (undefined :: IOVec)
                         , addrSz
                         , auxSz
                         , maxSz
                         ]
    mmsgPtr n ptr = ptr `plusPtr` (sizeOf (undefined :: MMsgHdr) * n)
    addrPtr n ptr = ptr `plusPtr` (sizeOf (undefined :: MMsgHdr) * maxNum + addrSz * n)
    iovPtr n ptr = ptr `plusPtr` (maxNum * sum [ sizeOf (undefined :: MMsgHdr)
                                               , addrSz
                                               ] + sizeOf (undefined :: IOVec) * n)
    auxPtr n ptr = ptr `plusPtr` (maxNum * sum [ sizeOf (undefined :: MMsgHdr)
                                               , addrSz
                                               , sizeOf (undefined :: IOVec)
                                               ] + auxSz * n)
    datPtr n ptr = ptr `plusPtr` (maxNum * sum [ sizeOf (undefined :: MMsgHdr)
                                               , addrSz
                                               , sizeOf (undefined :: IOVec)
                                               , auxSz
                                               ] + maxSz * n)

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
