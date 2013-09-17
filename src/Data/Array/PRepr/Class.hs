{-# LANGUAGE ExistentialQuantification, TypeFamilies, FlexibleContexts, UndecidableInstances #-}

module Data.Array.PRepr.Class where

import Control.Applicative
import Control.Monad.ST

data PArray a = PArray {-# UNPACK #-} !Int (PData a)

data PRDict a = PR a => PRDict

class PR a where
    type PData_ a
    unsafeDropPData :: Int -> PData a -> PData a
    unsafeIndexPData :: PData a -> Int -> a
    unsafeReplicatePData :: Int -> a -> PData a
    concatPData :: PData (Int, Int, Int) -> PData (PData a) -> PData a
    liftedInstance :: PRDict (PData a)

class PR a => Scalar a where
    data MPData s a
    unsafeNewMPData :: Int -> ST s (MPData s a)
    unsafeReadMPData :: MPData s a -> Int -> ST s a
    unsafeWriteMPData :: Int -> a -> MPData s a -> ST s ()
    unsafeFreezeMPData_ :: MPData s a -> ST s (PData_ a)

unsafeFreezeMPData :: Scalar a => MPData s a -> ST s (PData a)
unsafeFreezeMPData dat = PData <$> unsafeFreezeMPData_ dat

----------------- Injective PData ---------------------

newtype PData a = PData {unPData :: PData_ a}

instance PR (PData_ a) => PR (PData a) where
    type PData_ (PData a) = PData (PData_ a)
    unsafeDropPData n (PData dat) = PData (unsafeDropPData n dat)
    unsafeIndexPData (PData dat) i = PData (unsafeIndexPData dat i)
    unsafeReplicatePData n (PData dat) = PData (unsafeReplicatePData n dat)
    concatPData vsegs (PData dat) = PData (concatPData vsegs dat)
    liftedInstance = change liftedInstance where
        change :: PRDict (PData (PData_ a)) -> PRDict (PData (PData a))
        change PRDict = PRDict
