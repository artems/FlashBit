{-# LANGUAGE ScopedTypeVariables #-}

module Data.PieceSet.Test (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.PieceSet as PS

tests :: TestTree
tests = testGroup "PieceSet" [unitTests]

unitTests :: TestTree
unitTests = testGroup "Unit tests"
    [ testCase "(toList . fromList) x = x" $ do
        let n = 20
            ps0 = [5, 10, 15]
        ps1 <- PS.fromList n ps0
        ps2 <- PS.toList ps1
        ps2 @?= ps0
    ,  testCase "#null" $ do
        ps <- PS.new 10
        isNull <- PS.null ps
        isNull @?= True
    ,  testCase "#full" $ do
        ps <- PS.fromList 3 [0, 1, 2]
        isFull <- PS.full ps
        isFull @?= True
    ,  testCase "#size" $ do
        ps <- PS.fromList 20 [1, 5, 15]
        size <- PS.size ps
        size @?= 3
    ,  testCase "#exist" $ do
        ps <- PS.fromList 20 [1, 5, 15]
        is0Exist <- PS.exist 0 ps
        is1Exist <- PS.exist 1 ps
        is0Exist @?= False
        is1Exist @?= True
    ,  testCase "#have" $ do
        ps <- PS.fromList 20 [1, 5, 15]
        PS.have 0 ps
        size <- PS.size ps
        isExist <- PS.exist 0 ps
        size @?= 4
        isExist @?= True
    ,  testCase "#unhave" $ do
        ps <- PS.fromList 20 [1, 5, 15]
        PS.unhave 1 ps
        size <- PS.size ps
        isExist <- PS.exist 1 ps
        size @?= 2
        isExist @?= False
    ,  testCase "#freeze + #exist'" $ do
        ps <- PS.fromList 20 [1, 5, 15]
        ps' <- PS.freeze ps
        PS.exist' 0 ps' @?= False
        PS.exist' 1 ps' @?= True
    ]
