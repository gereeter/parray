{-# LANGUAGE TypeFamilies #-}

module Data.Array.PRepr.Tuple where

import Data.Array.PRepr.Class

import Control.Applicative

instance PR () where
    type PData_ () = ()
    unsafeDropPData _ _ = PData ()
    unsafeIndexPData _ _ = ()
    unsafeReplicatePData _ _ = PData ()
    concatPData _ _ = PData ()
    liftedInstance = PRDict

instance Scalar () where
    data MPData s () = MATuple0
    unsafeNewMPData _ = return MATuple0
    unsafeReadMPData _ _ = return ()
    unsafeWriteMPData _ _ _ = return ()
    unsafeFreezeMPData_ _ = return ()

newtype Id a = Id {unId :: a}

instance PR a => PR (Id a) where
    type PData_ (Id a) = Id (PData a)
    unsafeDropPData n (PData (Id dat)) = PData (Id (unsafeDropPData n dat))
    unsafeIndexPData (PData (Id dat)) i = Id (unsafeIndexPData dat i)
    unsafeReplicatePData n (Id a) = PData (Id (unsafeReplicatePData n a))
    concatPData vsegs (PData (PData (Id dat))) = PData (Id (concatPData vsegs dat))
    liftedInstance = change liftedInstance where
        change :: PRDict (PData a) -> PRDict (PData (Id a))
        change PRDict = PRDict

instance (PR a, PR b) => PR (a, b) where
    type PData_ (a, b) = (PData a, PData b)
    unsafeDropPData n (PData (as, bs)) = PData (unsafeDropPData n as, unsafeDropPData n bs)
    unsafeIndexPData (PData (as, bs)) i = (unsafeIndexPData as i, unsafeIndexPData bs i)
    unsafeReplicatePData n (a, b) = PData (unsafeReplicatePData n a, unsafeReplicatePData n b)
    concatPData vsegs (PData (PData (ass, bss))) = PData (concatPData vsegs ass, concatPData vsegs bss)
    liftedInstance = change liftedInstance liftedInstance where
        change :: PRDict (PData a) -> PRDict (PData b) -> PRDict (PData (a, b))
        change PRDict PRDict = PRDict

instance (Scalar a, Scalar b) => Scalar (a, b) where
    data MPData s (a, b) = MATuple2 (MPData s a) (MPData s b)
    unsafeNewMPData n = MATuple2 <$> unsafeNewMPData n <*> unsafeNewMPData n
    unsafeReadMPData (MATuple2 as bs) i = (,) <$> unsafeReadMPData as i <*> unsafeReadMPData bs i
    unsafeWriteMPData i (a, b) (MATuple2 as bs) = unsafeWriteMPData i a as >> unsafeWriteMPData i b bs
    unsafeFreezeMPData_ (MATuple2 as bs) = (,) <$> unsafeFreezeMPData as <*> unsafeFreezeMPData bs

instance (PR a, PR b, PR c) => PR (a, b, c) where
    type PData_ (a, b, c) = (PData a, PData b, PData c)
    unsafeDropPData n (PData (as, bs, cs)) = PData (unsafeDropPData n as, unsafeDropPData n bs, unsafeDropPData n cs)
    unsafeIndexPData (PData (as, bs, cs)) i = (unsafeIndexPData as i, unsafeIndexPData bs i, unsafeIndexPData cs i)
    unsafeReplicatePData n (a, b, c) = PData (unsafeReplicatePData n a, unsafeReplicatePData n b, unsafeReplicatePData n c)
    concatPData vsegs (PData (PData (ass, bss, css))) = PData (concatPData vsegs ass, concatPData vsegs bss, concatPData vsegs css)
    liftedInstance = change liftedInstance liftedInstance liftedInstance where
        change :: PRDict (PData a) -> PRDict (PData b) -> PRDict (PData c) -> PRDict (PData (a, b, c))
        change PRDict PRDict PRDict = PRDict

instance (Scalar a, Scalar b, Scalar c) => Scalar (a, b, c) where
    data MPData s (a, b, c) = MATuple3 (MPData s a) (MPData s b) (MPData s c)
    unsafeNewMPData n = MATuple3 <$> unsafeNewMPData n <*> unsafeNewMPData n <*> unsafeNewMPData n
    unsafeReadMPData (MATuple3 as bs cs) i = (,,) <$> unsafeReadMPData as i <*> unsafeReadMPData bs i <*> unsafeReadMPData cs i
    unsafeWriteMPData i (a, b, c) (MATuple3 as bs cs) = unsafeWriteMPData i a as >> unsafeWriteMPData i b bs >> unsafeWriteMPData i c cs
    unsafeFreezeMPData_ (MATuple3 as bs cs) = (,,) <$> unsafeFreezeMPData as <*> unsafeFreezeMPData bs <*> unsafeFreezeMPData cs
