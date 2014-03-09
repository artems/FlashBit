module BCodeSpec (spec) where

import BCode

import Test.Hspec
import qualified Data.Map as M
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8


spec :: Spec
spec = do
    describe "decode" $ do
        it "should decode an integer" $ do
             decodeSpec "i10e" $ bint 10

        it "should decode a negative integer" $ do
             decodeSpec "i-5e" $ bint (-5)

        it "should decode a zero" $ do
             decodeSpec "i0e" $ bint 0

        it "should decode a string" $ do
             decodeSpec "4:spam" $ bstr "spam"

        it "should decode an empty string" $ do
             decodeSpec "0:" $ bstr ""

        it "should decode a string with spaces" $ do
             decodeSpec "9:ping pong" $ bstr "ping pong"

        it "should decode a list" $ do
             decodeSpec "li10e4:spame" $ blist [bint 10, bstr "spam"]

        it "should decode an empty list" $ do
             decodeSpec "le" $ blist []

        it "should decode a dictionary" $ do
             decodeSpec "d2:a1i10e2:a24:spame" $
                bdict [("a1", bint 10), ("a2", bstr "spam")]

        it "should decode an empty dictionary" $ do
             decodeSpec "de" $ bdict []

    describe "encode" $ do
        it "should encode an integer" $ do
            encodeSpec (bint 10) "i10e"

        it "should encode a negative integer" $ do
            encodeSpec (bint (-5)) "i-5e"

        it "should encode zero" $ do
            encodeSpec (bint 0) "i0e"

        it "should encode a string" $ do
            encodeSpec (bstr "spam") "4:spam"

        it "should encode an empty string" $ do
            encodeSpec (bstr "") "0:"

        it "should encode a string with spaces" $ do
            encodeSpec (bstr "ping pong") "9:ping pong"

        it "should encode a list" $ do
             encodeSpec (blist [bint 10, bstr "spam"]) "li10e4:spame"

        it "should encode an empty list" $ do
             encodeSpec (blist []) "le"

        it "should encode a dictionary" $ do
            encodeSpec (bdict [("a1", bint 10), ("a2", bstr "spam")]) "d2:a1i10e2:a24:spame"

        it "should encode an empty dictionary" $ do
             encodeSpec (bdict []) "de"

    describe "search" $ do
        it "should search the item by index" $ do
            search [BCodePInt 0] (blist [bint 5, bint 6]) `shouldBe` Just (bint 5)
            search [BCodePInt 1] (blist [bint 5, bint 6]) `shouldBe` Just (bint 6)
            search [BCodePInt 2] (blist [bint 5, bint 6]) `shouldBe` Nothing

        it "should search the item by key" $ do
            search [BCodePStr "a1"] (bdict [("a1", bint 10), ("a2", bstr "spam")]) `shouldBe` Just (bint 10)
            search [BCodePStr "a3"] (bdict [("a1", bint 10), ("a2", bstr "spam")]) `shouldBe` Nothing

        it "should return Nothing if path failed" $ do
            search [BCodePInt 0] (bint 1) `shouldBe` Nothing
            search [BCodePInt 1] (bint 1) `shouldBe` Nothing
            search [BCodePStr "a1"] (bstr "a0") `shouldBe` Nothing
            search [BCodePStr "a1"] (bstr "a1") `shouldBe` Nothing


-- utils
bint :: Integer -> BCode
bint = BInt

bstr :: String -> BCode
bstr = BStr . str2bs

blist :: [BCode] -> BCode
blist = BList

bdict :: [(String, BCode)] -> BCode
bdict = BDict . M.fromList . map (\(x, y) -> (str2bs x, y))

str2bs :: String -> B.ByteString
str2bs = B8.pack

encodeSpec :: BCode -> String -> Expectation
encodeSpec a b = encode a `shouldBe` (str2bs b)

decodeSpec :: String -> BCode -> Expectation
decodeSpec a b = a' `shouldBe` b
  where (Right a') = decode (str2bs a)


