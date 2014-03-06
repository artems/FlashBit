module Digest
    ( digest
    , digestlazy
    ) where


import qualified Crypto.Hash.SHA1 as SHA1

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL


digest :: B.ByteString -> B.ByteString
digest bs = SHA1.hash bs

digestlazy :: BL.ByteString -> B.ByteString
digestlazy bs = SHA1.hashlazy bs

