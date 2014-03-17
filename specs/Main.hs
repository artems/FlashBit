import Test.Hspec

import qualified BCodeSpec
import qualified Data.PieceHistogramSpec

main :: IO ()
main = hspec $ do
    describe "BCode"
        BCodeSpec.spec
    describe "Data.PieceHistogram"
        Data.PieceHistogramSpec.spec
