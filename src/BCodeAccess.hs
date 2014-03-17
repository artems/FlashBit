{-# LANGUAGE FlexibleInstances #-}

module BCodeAccess
    ( BCodeAccess(..)
    , getNumber
    , getString
    , getList
    ) where

import Data.List (genericIndex, genericLength)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8

import qualified Data.Map as M

import BCode


getI :: Int -> BCode -> Maybe BCode
getI index bc = case bc of
    (BList a) -> if index > 0 || index < length a
                    then Just (a !! index)
                    else Nothing
    _         -> Nothing

getI' :: Integer -> BCode -> Maybe BCode
getI' index bc = case bc of
    (BList a) -> if index > 0 || index < genericLength a
                    then Just (a `genericIndex` index)
                    else Nothing
    _         -> Nothing

getS :: String -> BCode -> Maybe BCode
getS key bc = case bc of
    (BDict a) -> M.lookup (B8.pack key) a
    _         -> Nothing

getBS :: B.ByteString -> BCode -> Maybe BCode
getBS key bc = case bc of
    (BDict a) -> M.lookup key a
    _         -> Nothing

getNumber :: BCode -> Maybe Integer
getNumber bc = case bc of
    BInt a -> Just a
    _      -> Nothing

getString :: BCode -> Maybe B.ByteString
getString bc = case bc of
    BStr a -> Just a
    _      -> Nothing

getList :: BCode -> Maybe [BCode]
getList bc = case bc of
    BList a -> Just a
    _       -> Nothing


class BCodeAccess a where
    get :: a -> BCode -> Maybe BCode

instance BCodeAccess Int where
    get = getI

instance BCodeAccess Integer where
    get = getI'

-- FlexibleInstances
instance BCodeAccess String where
    get = getS

instance BCodeAccess B.ByteString where
    get = getBS


