module Rate.Test (tests) where

import Data.Time.Clock
import Test.Tasty
import Test.Tasty.HUnit

import Rate


tests :: TestTree
tests = testGroup "Rate" [unitTests]

unitTests :: TestTree
unitTests = testGroup "Unit tests"
    [ testCase "extract rate" $ do
        ct <- getCurrentTime
        let ct1min = addUTCTime (fromIntegral (60 :: Int)) ct
        let (_, speed, _) = extractRate ct1min $ updateBytes 1024 $ mkRate ct

        -- 1024 / 60 ~= 17
        floor speed @?= (17 :: Int)
    , testCase "average speed" $ do
        ct <- getCurrentTime
        let ct1min = addUTCTime (fromIntegral (60 :: Int)) ct
            ct2min = addUTCTime (fromIntegral (60 :: Int)) ct1min
        let (_, _, rate)  = extractRate ct1min $ updateBytes 1024 $ mkRate ct
            (_, speed, _) = extractRate ct2min $ updateBytes 2048 $ rate

        -- 3072 / 120 ~= 25; 2048 / 60 ~= 34; (25 + 34) / 2 ~= 29
        assertBool "" (floor speed > (25 :: Int) && floor speed < (34 :: Int))
    ]
