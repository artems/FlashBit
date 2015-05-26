module Data.Queue
    ( Queue
    -- * Query
    , null       -- :: Queue a -> Bool
    , size       -- :: Queue a -> Int

    -- * Construction
    , empty      -- :: Queue a
    , singleton  -- :: a -> Queue a
    , fromList   -- :: [a] -> Queue a
    , Data.Foldable.toList     -- :: Queue a -> [a]

    -- * Manipulations
    , add        -- :: a -> Queue a -> Queue a
    , poll       -- :: Queue a -> Maybe (a, Queue a)
    , peek       -- :: Queue a -> Maybe a

    -- * Transformations
    , append     -- :: Queue a -> Queue a -> Queue a
    , filter     -- :: (a -> Bool) -> Queue a -> Queue a
    , partition  -- :: (a -> Bool) -> Queue a -> (Queue a, Queue a)
    ) where

import Prelude hiding (null, filter, foldr, foldl)
import qualified Data.List as L
import Data.Monoid (Monoid(..))
import Data.Foldable hiding (concat)
import Data.Traversable
import Control.Applicative (Applicative(..), (<$>), (<*>))

-- | General-purpose queues
data Queue a = Queue [a] [a]

instance Functor Queue where
    fmap f (Queue front back) = Queue (fmap f front) (fmap f back)

instance Foldable Queue where
    foldr f z (Queue front back) = foldr f (foldl (flip f) z front) back

instance Traversable Queue where
    traverse f (Queue front back) = (\bk fr -> Queue (reverse fr) bk) <$> traverse f back <*> traverse f (reverse front)

instance Monoid (Queue a) where
    mempty = empty
    mappend = append

instance (Eq a) => Eq (Queue a) where
    queueL == queueR = toList queueL == toList queueR

instance (Show a) => Show (Queue a) where
    show queue = "Queue " ++ show (toList queue)

-- | Is this the empty queue?
null :: Queue a -> Bool
null (Queue [] []) = True
null _             = False

-- | The number of elements in the queue.
size :: Queue a -> Int
size (Queue front back) = length front + length back

-- | The empty queue.
empty :: Queue a
empty = Queue [] []

-- | A singleton queue.
singleton :: a -> Queue a
singleton x = Queue [x] []

-- | Create a queue from a finite list of elements.
fromList :: [a] -> Queue a
fromList xs = Queue (reverse xs) []

-- | Add an element to the queue.
add :: a -> Queue a -> Queue a
add x (Queue front back) = Queue front (x : back)

-- | Retrieves and removes the head of the queue.
poll :: Queue a -> Maybe (a, Queue a)
poll (Queue [] []) = Nothing
poll (Queue [] back) = poll (Queue (reverse back) [])
poll (Queue (x : xs) back) = Just (x, Queue xs back)

-- | Retrieves, but does not remove, the head of the queue.
peek :: Queue a -> Maybe a
peek (Queue [] []) = Nothing
peek (Queue [] back) = peek (Queue (reverse back) [])
peek (Queue (x : _) _) = Just x

-- | Append two queues.
append :: Queue a -> Queue a -> Queue a
append (Queue lfront lback) (Queue rfront rback) = Queue front back
  where
    front = rfront ++ reverse rback ++ lfront
    back  = lback

-- | Returns the queue of those elements that satisfy the predicate.
filter :: (a -> Bool) -> Queue a -> Queue a
filter f (Queue front back) = Queue (L.filter f front) (L.filter f back)

-- | Returns the pair of queues of elements which do and do not satisfy the predicate, respectively.
partition :: (a -> Bool) -> Queue a -> (Queue a, Queue a)
partition f (Queue front back) =
    let (fl, fr) = L.partition f front
        (bl, br) = L.partition f back
     in (Queue fl bl, Queue fr br)
