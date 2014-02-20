module Protocol.PeerSpec (spec) where

import Protocol.Peer
-- import Protocol.Types

import Test.Hspec
import qualified Data.ByteString as B
-- import qualified Data.ByteString.Char8 as B8


spec :: Spec
spec = do
    describe "buildBitField" $ do
        it "should build bitfield" $ do
            -- 1100 1000 =>
            buildBitField 5 [0, 1, 4]  `shouldBe` B.pack [200]
            -- 0001 0100 0100 0000 =>
            buildBitField 12 [3, 5, 9]  `shouldBe` B.pack [20, 64]

 
