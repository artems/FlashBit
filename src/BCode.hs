module BCode
    ( BCode(..)
    , BCodePath(..)
    , encode
    , decode
    , search
    , searchInt
    , searchStr
    ) where

import Control.Applicative ((<$>), (<*>), (<|>), (*>), (<*))

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import Data.Char (ord, isDigit, digitToInt)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Word (Word8)

import Text.Parsec.ByteString (Parser)
import qualified Text.Parsec.Char as P
import qualified Text.Parsec.Error as P
import qualified Text.Parsec.Combinator as P
import qualified Text.ParserCombinators.Parsec.Prim as P


data BCode
    = BInt Integer
    | BStr ByteString
    | BList [BCode]
    | BDict (Map ByteString BCode)
    deriving (Show, Read, Ord, Eq)


instance Eq P.ParseError where
    _ == _ = False


toW8 :: Char -> Word8
toW8 = fromIntegral . ord

wrap :: Char -> Char -> ByteString -> ByteString
wrap a b c = B.concat [B8.pack [a], c, B8.pack [b]]

between :: Char -> Char -> Parser a -> Parser a
between a b c = P.char a *> c <* P.char b


getUInt :: (Num a) => Parser a
getUInt = fromIntegral <$> digit
  where
    digit = str2int <$> P.many1 (P.satisfy isDigit)
    str2int = foldl (\acc x -> acc * 10 + digitToInt x) 0


getBInt :: Parser BCode
getBInt = BInt <$> between 'i' 'e' getInt
  where
    sign = (P.char '-' >> return negate) <|> return id
    getInt = sign <*> getUInt


getBStr :: Parser BCode
getBStr = do
    count <- getUInt
    P.char ':'
    BStr . B8.pack <$> P.count count P.anyToken


getBList :: Parser BCode
getBList = BList <$> between 'l' 'e' (P.many getBCode)


getBDict :: Parser BCode
getBDict = BDict . M.fromList <$> between 'd' 'e'  (P.many getPair)
  where
    getPair = do
        (BStr key) <- getBStr
        value <- getBCode
        return (key, value)


getBCode :: Parser BCode
getBCode = getBInt <|> getBStr <|> getBList <|> getBDict


decode :: ByteString -> Either P.ParseError BCode
decode input = P.parse getBCode "" input


encode :: BCode -> ByteString
encode = put
  where
    put (BInt i) = wrap 'i' 'e' $ int2bs i
    put (BStr s) = B.concat [int2bs $ B.length s, B8.pack ":", s]
    put (BList l) = wrap 'l' 'e' $ B.concat (map put l)
    put (BDict d) = wrap 'd' 'e' $ B.concat (map encodePair $ M.toList d)
    int2bs :: (Integral a) => a -> ByteString
    int2bs = B8.pack . show . fromIntegral
    encodePair :: (ByteString, BCode) -> ByteString
    encodePair (k, v) = put (BStr k) `B.append` put v


data BCodePath = BCodePInt Int
               | BCodePStr String

search :: [BCodePath] -> BCode -> Maybe BCode
search [] a = Just a
search (BCodePInt x:xs) (BList l)
    | x < 0 || (x + 1) > length l = Nothing
    | otherwise = search xs (l !! x)
search (BCodePStr x:xs) (BDict d) =
    M.lookup (B8.pack x) d >>= search xs
search _ _ = Nothing

searchInt :: String -> BCode -> Maybe Integer
searchInt key bc = do
    (BInt i) <- search [BCodePStr key] bc
    return i

searchStr :: String -> BCode -> Maybe ByteString
searchStr key bc = do
    (BStr s) <- search [BCodePStr key] bc
    return s

