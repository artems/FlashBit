module Network.URI.Extra.Test (tests) where

import qualified Data.ByteString.Char8 as B8
import Test.Tasty
import Test.Tasty.HUnit

import Network.URI.Extra


tests :: TestTree
tests = testGroup "URI" [unitTests]

unitTests :: TestTree
unitTests = testGroup "Unit tests"
    [ testGroup "urlEnode"
        [ testCase "empty" $
            urlEncode B8.empty @?= ""
        , testCase "alphabet chars" $
            urlEncode (B8.pack "abcdef") @?= "abcdef"
        , testCase "special chars" $
            urlEncode (B8.pack "?&(){}") @?= "%3f%26%28%29%7b%7d"
        ]
    , testGroup "urlEncodeVars"
        [ testCase "empty list" $
            urlEncodeVars [] @?= ""
        , testCase "only one pair" $
            urlEncodeVars [(B8.pack "one", B8.pack "1")] @?= "one=1"
        , testCase "have 2 values for one name" $
            urlEncodeVars [(B8.pack "one", B8.pack "a"), (B8.pack "one", B8.pack "b")] @?= "one=a,b"
        , testCase "several items" $
            urlEncodeVars [(B8.pack "one", B8.pack "a"), (B8.pack "two", B8.pack "b")] @?= "one=a&two=b"
        ]
    ]
