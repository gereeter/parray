{-# LANGUAGE TypeFamilies, CPP #-}

module Data.Array.PRepr.Primitive where

import Data.Array.PRepr.Class

import Control.Applicative
import Control.Monad (forM_)

import Control.Monad.ST

import Data.Primitive.Array
import Data.Primitive.ByteArray

import Data.Int
import Data.Word

data OffArray a = OffArray {-# UNPACK #-} !Int {-# UNPACK #-} !(Array a)
data OffByteArray = OffByteArray {-# UNPACK #-} !Int {-# UNPACK #-} !ByteArray

instance PR (OffArray a) where
    type PData_ (OffArray a) = OffArray (OffArray a)
    unsafeDropPData n (PData (OffArray off dat)) = PData (OffArray (off + n) dat)
    unsafeIndexPData (PData (OffArray off dat)) i = indexArray dat (off + i)
    unsafeReplicatePData n x = runST $ do
        mba <- newArray n x
        PData . OffArray 0 <$> unsafeFreezeArray mba
    liftedInstance = PRDict

instance PR OffByteArray where
    type PData_ OffByteArray = OffArray OffByteArray
    unsafeDropPData n (PData (OffArray off dat)) = PData (OffArray (off + n) dat)
    unsafeIndexPData (PData (OffArray off dat)) i = indexArray dat (off + i)
    unsafeReplicatePData n x = runST $ do
        mba <- newArray n x
        PData . OffArray 0 <$> unsafeFreezeArray mba
    liftedInstance = PRDict

#define PrimInstance(T)                                                                   \
instance PR T where {                                                                     \
;   type PData_ T = OffByteArray                                                          \
;   unsafeDropPData n (PData (OffByteArray off dat)) = PData (OffByteArray (off + n) dat) \
;   unsafeIndexPData (PData (OffByteArray off dat)) i = indexByteArray dat (off + i)      \
;   unsafeReplicatePData n x = runST $ do {                                               \
;       mba <- newByteArray n                                                             \
;       forM_ [0..n-1] $ \i -> writeByteArray mba i x                                     \
;       PData . OffByteArray 0 <$> unsafeFreezeByteArray mba }                            \
;   liftedInstance = PRDict }

PrimInstance(Int)
PrimInstance(Int8)
PrimInstance(Int16)
PrimInstance(Int32)
PrimInstance(Int64)
PrimInstance(Word)
PrimInstance(Word8)
PrimInstance(Word16)
PrimInstance(Word32)
PrimInstance(Word64)
