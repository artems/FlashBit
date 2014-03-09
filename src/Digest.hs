module Digest
    ( digest
    , digestlazy
    ) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL

import qualified Crypto.Hash.SHA1 as SHA1


digest :: B.ByteString -> B.ByteString
digest bs = SHA1.hash bs

digestlazy :: BL.ByteString -> B.ByteString
digestlazy bs = SHA1.hashlazy bs
