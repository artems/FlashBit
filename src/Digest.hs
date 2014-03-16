module Digest
    ( digest
    ) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL

import qualified Crypto.Hash.SHA1 as SHA1


digest :: B.ByteString -> B.ByteString
digest bs = SHA1.hash bs
