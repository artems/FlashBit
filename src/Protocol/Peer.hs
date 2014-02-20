module Protocol.Peer
    ( Message(..)
    , decodeMessage
    , encodeMessage
    , Handshake(..)
    , decodeHandshake
    , encodeHandshake
    , buildBitField
    ) where


import Control.Applicative ((*>), (<$>), (<*>))
import Control.Monad (when, unless)

import Data.Binary (Binary, put, get, encode, decode)
import Data.Binary.Get (Get)
import qualified Data.Binary.Get as Get
import Data.Binary.Put (Put)
import qualified Data.Binary.Put as Put

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as B8

import Data.Word

import Prelude hiding (length)

import Protocol.Types hiding (Piece)


data Message
    = KeepAlive
    | Choke
    | Unchoke
    | Interested
    | NotInterested
    | Have PieceNum
    | BitField ByteString
    | Request PieceNum PieceBlock
    | Piece PieceNum PieceBlockOffset ByteString
    | Cancel PieceNum PieceBlock
    | Port Word16
    deriving (Eq, Show)

instance Binary Message where
    get = getMessage
    put a = p32be (messageSize a) *> putMessage a


data Handshake = Handshake PeerId InfoHash [Capabilities]

instance Binary Handshake where
    get = getHandshake
    put a = putHandshake a


p8 :: Word8 -> Put
p8 = Put.putWord8

pBS :: ByteString -> Put
pBS = Put.putByteString

p16be :: (Integral a) => a -> Put
p16be = Put.putWord16be . fromIntegral

p32be :: (Integral a) => a -> Put
p32be = Put.putWord32be . fromIntegral


g8 :: Get Word8
g8 = Get.getWord8

gBS :: Word32 -> Get ByteString
gBS = Get.getByteString . fromIntegral

g16be :: (Integral a) => Get a
g16be = fromIntegral <$> Get.getWord16be

g32be :: (Integral a) => Get a
g32be = fromIntegral <$> Get.getWord32be

g64be :: (Integral a) => Get a
g64be = fromIntegral <$> Get.getWord64be


getMessage :: Get Message
getMessage = do
    length <- g32be
    if length == 0
        then return KeepAlive
        else getMessage' length

getMessage' :: Word32 -> Get Message
getMessage' length = do
    messageId <- g8
    case messageId of
        0 -> check (==  1) >> return Choke
        1 -> check (==  1) >> return Unchoke
        2 -> check (==  1) >> return Interested
        3 -> check (==  1) >> return NotInterested
        4 -> check (==  5) >> Have     <$> g32be
        5 -> check (>   1) >> BitField <$> gBS (length - 1)
        6 -> check (== 13) >> Request  <$> g32be <*> (PieceBlock <$> g32be <*> g32be)
        7 -> check (>   9) >> Piece    <$> g32be <*> g32be <*> gBS (length - 9)
        8 -> check (== 13) >> Cancel   <$> g32be <*> (PieceBlock <$> g32be <*> g32be)
        9 -> check (==  3) >> Port     <$> g16be
        a -> fail $ "illegal message id: " ++ show a
  where
    check :: (Word32 -> Bool) -> Get ()
    check test = do
        when (test length) (fail "wrong message size")
        return ()


putMessage :: Message -> Put
putMessage a = case a of
    KeepAlive       -> return ()
    Choke           -> p8 0
    Unchoke         -> p8 1
    Interested      -> p8 2
    NotInterested   -> p8 3
    Have pieceNum   -> p8 4 *> p32be pieceNum
    BitField bitfield
                    -> p8 5 *> pBS bitfield
    Request pieceNum (PieceBlock offset length)
                    -> p8 6 *> mapM_ p32be [pieceNum, offset, length]
    Piece pieceNum offset content
                    -> p8 7 *> mapM_ p32be [pieceNum, offset] *> pBS content
    Cancel pieceNum (PieceBlock offset length)
                    -> p8 8 *> mapM_ p32be [pieceNum, offset, length]
    Port port       -> p8 9 *> p16be port


messageSize :: Message -> Word8
messageSize m = case m of
    KeepAlive       -> 0
    Choke           -> 1
    Unchoke         -> 1
    Interested      -> 1
    NotInterested   -> 1
    (Have _)        -> 5
    (BitField a)    -> fromIntegral (B.length a) + 1
    (Request _ _)   -> 13
    (Piece _ _ a)   -> fromIntegral (B.length a) + 9
    (Cancel _ _)    -> 13
    (Port _)        -> 3


header :: B.ByteString
header = B8.pack "BitTorrent protocol"

headerSize :: (Num a) => a
headerSize = fromIntegral $ B.length header

handshakeSize :: Integer
handshakeSize
    = 1  -- length
    + headerSize
    + 8  -- capabilities
    + 20 -- info_hash
    + 20 -- peer_id

encodeCapabilities :: [Capabilities] -> Word64
encodeCapabilities _ = 0

decodeCapabilities :: Word64 -> [Capabilities]
decodeCapabilities _ = []

getHandshake :: Get Handshake
getHandshake =  do
    protocolHeaderSize <- g8
    when (protocolHeaderSize /= headerSize) $
        fail "wrong header size"
    protocolHeader <- gBS headerSize
    when (protocolHeader /= header) $
        fail "wrong protocol header"
    capabilities <- decodeCapabilities `fmap` g64be
    infoHash <- gBS 20
    peerId <- B8.unpack `fmap` gBS 20
    return (Handshake peerId infoHash capabilities)


putHandshake :: Handshake -> Put
putHandshake (Handshake peerId infoHash capabilities) = do
    Put.putWord8 headerSize
    Put.putByteString header
    Put.putWord64be (encodeCapabilities capabilities)
    Put.putByteString infoHash
    Put.putByteString (B8.pack peerId)


incDecoder :: Get a -> IO ByteString -> Maybe ByteString -> String
           -> IO (ByteString, a)
incDecoder parser drain bs errPrefix = do
    let decoder = Get.runGetIncremental parser
    case bs of
        Just bs -> loop (decoder `Get.pushChunk` bs)
        Nothing -> loop decoder
  where
    loop (Get.Fail _ _ e) = ioError (userError $ errPrefix ++ ": " ++ e)
    loop (Get.Done bs _ a) = return (bs, a)
    loop (Get.Partial feed) = drain >>= (\bs -> loop (feed (Just bs)))


encodeMessage :: Message -> ByteString
encodeMessage m = BL.toStrict (encode m)

decodeMessage :: ByteString -> IO ByteString -> IO (ByteString, Message)
decodeMessage bs drain = incDecoder getMessage drain (Just bs) "decodeMessage"

encodeHandshake :: Handshake -> ByteString
encodeHandshake handshake = BL.toStrict (encode handshake)

decodeHandshake :: IO ByteString -> IO (ByteString, Handshake)
decodeHandshake drain = incDecoder getHandshake drain Nothing "decodeHandshake"


buildBitField :: Integer -> [PieceNum] -> ByteString
buildBitField size pieces = B.pack (build piecemap)
  where
    piecemap = map (`elem` pieces) [0 .. size - 1 + pad]
      where
        pad = case (size `mod` 8) of { 0 -> 0; n -> 8 - n }

    build [] = []
    build xs = let
        (first, rest) = splitAt 8 xs
        in bytify first : build rest

    bytify [b7, b6, b5, b4, b3, b2, b1, b0] = sum
        [ if b0 then 1 else 0
        , if b1 then 2 else 0
        , if b2 then 4 else 0
        , if b3 then 8 else 0
        , if b4 then 16 else 0
        , if b5 then 32 else 0
        , if b6 then 64 else 0
        , if b7 then 128 else 0
        ]
    bytify _ = error "impossible"


