import Test.Hspec

import qualified BCodeSpec
import qualified Protocol.PeerSpec

main :: IO ()
main = hspec $ do
    describe "BCode"
        BCodeSpec.spec
    describe "Protocol.Peer"
        Protocol.PeerSpec.spec


