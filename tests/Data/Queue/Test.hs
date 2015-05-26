{-# LANGUAGE ScopedTypeVariables #-}

module Data.Queue.Test (tests) where

import Data.Monoid
import qualified Data.Foldable as F
import qualified Data.Traversable as T
import Control.Monad (foldM)
import qualified Control.Monad.State as S

import Test.Tasty
import qualified Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit

import Data.Queue as Q


instance (QC.Arbitrary a) => QC.Arbitrary (Queue a) where
    arbitrary = do
        cnt <- QC.arbitrary :: QC.Gen Int
        list <- QC.arbitrary :: (QC.Arbitrary a) => QC.Gen [a]
        foldM (\q _ -> genAction q) (fromList list) [1..cnt]
      where
        genAction q = do
            action <- QC.arbitrary
            if action then applyAdd q else applyPoll q
        applyAdd q = do
            arg <- QC.arbitrary
            return $ add arg q
        applyPoll q = case poll q of
            Nothing -> return q
            Just (_, q') -> return q'


tests :: TestTree
tests = testGroup "Queue" [properties, unitTests]

properties :: TestTree
properties = testGroup "Properties" [functor, monoid, foldable, traversable, fromListToList]

functor :: TestTree
functor = testGroup "Functor"
    [ QC.testProperty "fmap id = id" $
        \(queue :: Queue Int) -> fmap id queue == queue
    , QC.testProperty "fmap (p . q) = (fmap p) . (fmap q)" $
        \(queue :: Queue Int) ->
            fmap ((+1) . (*2)) queue == (fmap (+1) . fmap (*2)) queue
    ]

monoid :: TestTree
monoid = testGroup "Monoid"
    [ QC.testProperty "mappend mempty x = x" $
        \(x :: Queue Int) -> mappend mempty x == x
    , QC.testProperty "mappend x mempty = x" $
        \(x :: Queue Int) -> mappend x mempty == x
    , QC.testProperty "mappend x (mappend y z) = mappend (mappend x y) z" $
        \(x :: Queue Int) (y :: Queue Int) (z :: Queue Int) -> mappend x (mappend y z) == mappend (mappend x y) z
    , QC.testProperty "mconcat = foldr mappend mempty" $
        \(x :: Queue Int) (y :: Queue Int) (z :: Queue Int) -> mconcat [x, y, z] == foldr mappend mempty [x, y, z]
    ]

foldable :: TestTree
foldable = testGroup "Foldable"
    [ QC.testProperty "foldr f z = foldr f z . toList" $
        \(queue :: Queue Int) -> F.foldr (*) 0 queue == (F.foldr (*) 0 . toList) queue
    ]

traversable :: TestTree
traversable = testGroup "Traversable"
    [ QC.testProperty "traverse Identity = Identity" $
        \(queue :: Queue Int) -> T.traverse Just queue == Just queue
    , QC.testProperty "traverse from left to right" $
        \(queue :: Queue Int) -> reverse (S.execState (T.traverse (\x -> S.modify (x:)) queue) []) == toList queue
    ]

fromListToList :: TestTree
fromListToList = testGroup "fromList"
    [ QC.testProperty "(toList . fromList) x = x " $
        \(queue :: [Int]) -> (toList . fromList) queue == queue
    ]

unitTests :: TestTree
unitTests = testGroup "Unit tests"
    [ testCase "null empty = True" $
        Q.null Q.empty @?= True
    , testCase "null (singleton 1) = False" $
        Q.null (Q.singleton (1 :: Int)) @?= False
    , testCase "size empty = 0" $
        Q.size Q.empty @?= 0
    , testCase "size (singleton 1) = 1" $
        Q.size (Q.singleton (1 :: Int)) @?= 1
    , testCase "`add' should add element to left" $
        toList (Q.add 2 (Q.add 1 Q.empty)) @?= ([2, 1] :: [Int])
    , testCase "`poll' should return Nothing if queue is an empty" $
        Q.poll (Q.empty :: Queue Int)  @?= Nothing
    , testCase "`poll' should remove element from right" $
        Q.poll (Q.fromList ([1, 2, 3] :: [Int])) @?= Just (3, Q.fromList [1, 2])
    , testCase "`peek' should return rightmost element" $
        Q.peek (Q.fromList ([1, 2, 3] :: [Int])) @?= Just 3
    , testCase "`append'" $
        Q.append queueA queueB @?= Q.fromList [5, 4, 1, 2, 3, 0, 9, 6, 7, 8]
    , testCase "`filter'" $
        Q.filter odd queueA @?= Q.fromList [5, 1, 3]
    , testCase "`partition'" $
        Q.partition odd queueA @?= (Q.fromList [5, 1, 3], Q.fromList [4, 2])
    ]
  where
    queueA = Q.add 5 (Q.add 4 (Q.fromList [1, 2, 3])) :: Queue Int
    queueB = Q.add 0 (Q.add 9 (Q.fromList [6, 7, 8])) :: Queue Int
