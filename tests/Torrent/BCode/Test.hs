{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Torrent.BCode.Test (tests) where

import qualified Data.Map as M
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import Control.Applicative ((<$>))
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import Test.Tasty
import qualified Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit

import Torrent.BCode as BCode


instance QC.Arbitrary B.ByteString where
    arbitrary = fmap B.pack QC.arbitrary

instance QC.Arbitrary BCode where
    arbitrary = QC.sized bcode
      where
        bcode :: Int -> QC.Gen BCode.BCode
        bcode 0 = QC.oneof
            [ BCode.BInt <$> QC.arbitrary
            , BCode.BStr <$> QC.arbitrary
            ]
        bcode n = QC.oneof
            [ BCode.BInt <$> QC.arbitrary
            , BCode.BStr <$> QC.arbitrary
            , BCode.BList <$> sequence (replicate n $ bcode (n `div` 8))
            , do keys <- QC.vectorOf n QC.arbitrary
                 vals <- sequence (replicate n $ bcode (n `div` 8))
                 return $ BCode.BDict $ M.fromList $ zip keys vals
            ]

tests :: TestTree
tests = testGroup "Torrent.BCode" [properties, unitTests]

properties :: TestTree
properties = testGroup "Properties" [encodeDecode]

encodeDecode :: TestTree
encodeDecode = QC.testProperty "(decode . encode) x = (Right x) " $
    \(bcode :: BCode) -> (BCode.decode . BCode.encode) bcode == (Right bcode)

unitTests :: TestTree
unitTests = testGroup "Unit tests" [decodeTests, encodeTests]

decodeTests :: TestTree
decodeTests = testGroup "decode"
    [ testCase "it should decode an integer" $
        decodeTest "i10e" (bint 10)
    , testCase "it should decode a negative integer" $
        decodeTest "i-5e" (bint (-5))
    , testCase "it should decode a zero" $
        decodeTest "i0e" (bint 0)
    , testCase "it should decode an empty string" $
        decodeTest "0:" (bstr "")
    , testCase "it should decode a string" $
        decodeTest "4:spam" (bstr "spam")
    , testCase "it should decode a string with spaces" $
        decodeTest "9:ping pong" (bstr "ping pong")
    , testCase "it should decode an empty list" $
        decodeTest "le" (blist [])
    , testCase "it should decode a list" $
        decodeTest "li10e4:spame" (blist [bint 10, bstr "spam"])
    , testCase "it should decode an empty dictionary" $
        decodeTest "de" $ bdict []
    , testCase "it should decode a dictionary" $
        decodeTest "d2:a1i10e2:a24:spame" $
            bdict [("a1", bint 10), ("a2", bstr "spam")]
    , testCase "it should not corrupt utf-8 string" $
        decodeUTF8Test "8:Тест" $ B.pack [208, 162, 208, 181, 209, 129, 209, 130]
    ]

encodeTests :: TestTree
encodeTests = testGroup "encode"
    [ testCase "it should encode an integer" $
        encodeTest (bint 10) "i10e"
    , testCase "it should encode a negative integer" $
        encodeTest (bint (-5)) "i-5e"
    , testCase "it should encode zero" $
        encodeTest (bint 0) "i0e"
    , testCase "it should encode a string" $
        encodeTest (bstr "spam") "4:spam"
    , testCase "it should encode an empty string" $
        encodeTest (bstr "") "0:"
    , testCase "it should encode a string with spaces" $
        encodeTest (bstr "ping pong") "9:ping pong"
    , testCase "it should encode a list" $
         encodeTest (blist [bint 10, bstr "spam"]) "li10e4:spame"
    , testCase "it should encode an empty list" $
         encodeTest (blist []) "le"
    , testCase "it should encode a dictionary" $
        encodeTest (bdict [("a1", bint 10), ("a2", bstr "spam")]) "d2:a1i10e2:a24:spame"
    , testCase "it should encode an empty dictionary" $
         encodeTest (bdict []) "de"
    ]

bint :: Integer -> BCode.BCode
bint = BCode.BInt

bstr :: String -> BCode.BCode
bstr = BCode.BStr . str2bs

blist :: [BCode] -> BCode.BCode
blist = BCode.BList

bdict :: [(String, BCode.BCode)] -> BCode.BCode
bdict = BCode.BDict . M.fromList . map (\(x, y) -> (str2bs x, y))

str2bs :: String -> B.ByteString
str2bs = B8.pack

decodeTest :: String -> BCode.BCode -> Assertion
decodeTest a b = a' @?= b'
  where a' = BCode.decode (str2bs a)
        b' = (Right b)

decodeUTF8Test :: String -> B.ByteString -> Assertion
decodeUTF8Test a b = a' @?= b'
  where a' = BCode.decode (T.encodeUtf8 . T.pack $ a)
        b' = (Right . BCode.BStr $ b)

encodeTest :: BCode.BCode -> String -> Assertion
encodeTest a b = encode a @?= (str2bs b)
