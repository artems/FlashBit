module URI
    ( urlEncode
    , urlEncodeVars
    ) where


import Data.Char (ord)
import Data.List (partition)
import qualified Data.ByteString as B
import Text.Printf


urlEncode :: B.ByteString -> String
urlEncode url = concatMap tohex (B.unpack url)
  where
    tohex x
        | x > 47 && x < 58  = printf "%c" x
        | x > 64 && x < 91  = printf "%c" x
        | x > 96 && x < 123 = printf "%c" x
        | otherwise         = '%' : printf "%02x" x


urlEncodeVars :: [(B.ByteString, B.ByteString)] -> String
urlEncodeVars [] = []
urlEncodeVars ((n,v):t) =
    let (same, diff) = partition ((== n) . fst) t
    in  urlEncode n ++ '=' : foldl (\x y -> x ++ ',' : urlEncode y) (urlEncode v) (map snd same)
            ++ urlEncodeRest diff
  where
    urlEncodeRest []   = []
    urlEncodeRest diff = '&' : urlEncodeVars diff

