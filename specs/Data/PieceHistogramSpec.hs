module Data.PieceHistogramSpec (spec) where


import Data.PieceHistogram

import Test.Hspec


spec :: Spec
spec = do
    describe "pick" $ do
        it "should pick rarest piece" $ do
            let ps = allHave [1, 1, 2, 3, 3] empty
            pick (const True) 1 ps `shouldBe` [2]

