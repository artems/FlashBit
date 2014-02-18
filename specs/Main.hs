import Test.Hspec

import qualified BCodeSpec

main :: IO ()
main = hspec $ do
    describe "BCode"
        BCodeSpec.spec


