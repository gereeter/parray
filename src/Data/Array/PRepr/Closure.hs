{-# LANGUAGE ExistentialQuantification, TypeFamilies, FlexibleContexts, UndecidableInstances, ScopedTypeVariables, TypeOperators, RankNTypes, CPP #-}

module Data.Array.PArray where

import Control.Applicative
import Control.Monad (forM_)

import Control.Monad.ST

import Data.Primitive.Array
import Data.Primitive.ByteArray

import Data.Int
import Data.Word

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

-------------------- Primitives ------------------------

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

------------------- Tuples -----------------------------

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

------------- Nested -------------------

type ANested a = (PData Int, PData (Int, Int, Int), PData (PData a))

instance (PR a, PR (PData a)) => PR (PArray a) where
    type PData_ (PArray a) = ANested a
    unsafeDropPData n (PData (vsegIDs, vsegs, psegs)) = PData (unsafeDropPData n vsegIDs, vsegs, psegs)
    unsafeIndexPData (PData (vsegIDs, vsegs, psegs)) i = PArray len (unsafeDropPData start pseg) where
        vsegID = unsafeIndexPData vsegIDs i
        (psegID, start, len) = unsafeIndexPData vsegs vsegID
        pseg   = unsafeIndexPData psegs psegID
    unsafeReplicatePData n (PArray len pseg) = PData (unsafeReplicatePData n 0, unsafeReplicatePData 1 (0, 0, len), unsafeReplicatePData 1 pseg)
    -- concatPData segIDs starts lens (ANested )
    liftedInstance = change liftedInstance where
        change :: PRDict (PData (PData a)) -> PRDict (PData (PArray a))
        change PRDict = PRDict
    

------------ Closures ---------------

data Clo_ f a b = forall e . (PR e, PR (f e)) => Clo_ (f e) (e -> a -> b) (Int -> PData e -> PData a -> PData b)
clo_ :: PRDict e -> PRDict (g e) -> g e -> (e -> a -> b) -> (Int -> PData e -> PData a -> PData b) -> Clo_ g a b
clo_ PRDict PRDict = Clo_
data WithPData f a = (PR a, PR (f a)) => WithPData (PData (f a))

instance (PR a, PR (f a), PR (PData (f a))) => PR (WithPData f a) where
    type PData_ (WithPData f a) = WithPData PData (f a)
    unsafeDropPData n (PData (WithPData dat)) = PData (WithPData (unsafeDropPData n dat))
    unsafeIndexPData (PData (WithPData dat)) i = WithPData (unsafeIndexPData dat i)
    unsafeReplicatePData n (WithPData dat) = PData (WithPData (unsafeReplicatePData n dat))
    concatPData vsegs (PData (PData (WithPData dat))) = PData (WithPData (concatPData vsegs dat))
    liftedInstance = change PRDict PRDict liftedInstance where
        change :: PRDict (f a) -> PRDict (PData (f a)) -> PRDict (PData (PData (f a))) -> PRDict (PData (WithPData f a))
        change PRDict PRDict PRDict = PRDict

instance PR (Clo_ f a b) where
    type PData_ (Clo_ f a b) = Clo_ (WithPData f) a b
    unsafeDropPData n (PData (Clo_ (WithPData dat) fS fL)) = PData (Clo_ (WithPData (unsafeDropPData n dat)) fS fL)
    unsafeIndexPData (PData (Clo_ (WithPData dat) fS fL)) i = Clo_ (unsafeIndexPData dat i) fS fL
    unsafeReplicatePData n (Clo_ dat fS fL) = PData (clo_ PRDict (change PRDict PRDict liftedInstance) (WithPData (unsafeReplicatePData n dat)) fS fL) where
        change :: PRDict e -> PRDict (f e) -> PRDict (PData (f e)) -> PRDict (WithPData f e)
        change PRDict PRDict PRDict = PRDict
    concatPData vsegs (PData (PData (Clo_ (WithPData (PData (WithPData dat))) fS fL))) = PData (Clo_ (WithPData (concatPData vsegs dat)) fS fL)
    liftedInstance = PRDict

type (:->) = Clo_ Id
infixr :->

($:) :: (a :-> b) -> a -> b
Clo_ (Id e) fS _ $: x = fS e x

($:^) :: (a :-> b) -> PArray a -> PArray b
Clo_ (Id e) _ fL $:^ (PArray s as) = PArray s (fL s (unsafeReplicatePData s e) as)

closure1 :: (a -> b) -> (Int -> PData a -> PData b) -> (a :-> b)
closure1 fS fL = Clo_ (Id ()) (const fS) (\s -> const (fL s))

closure2 :: (PR a) => (a -> b -> c) -> (Int -> PData a -> PData b -> PData c) -> (a :-> b :-> c)
closure2 fS fL = closure1 (\a -> Clo_ (Id a) fS fL) (\_ as -> PData $ clo_ PRDict (change PRDict liftedInstance) (WithPData (PData (Id as))) fS fL) where
    change :: PRDict x -> PRDict (PData x) -> PRDict (WithPData Id x)
    change PRDict PRDict = PRDict

closure3 :: (PR a, PR b) => (a -> b -> c -> d) -> (Int -> PData a -> PData b -> PData c -> PData d) -> (a :-> b :-> c :-> d)
closure3 fS fL = closure2 (\a b -> Clo_ (Id (a, b)) fS' fL') (\_ as bs -> PData $ clo_ PRDict (change PRDict liftedInstance) (WithPData (PData (Id (PData (as, bs))))) fS' fL') where
    fS' (a, b) = fS a b
    fL' s (PData (as, bs)) = fL s as bs
    change :: PRDict x -> PRDict (PData x) -> PRDict (WithPData Id x)
    change PRDict PRDict = PRDict

closure4 :: (PR a, PR b, PR c) => (a -> b -> c -> d -> e) -> (Int -> PData a -> PData b -> PData c -> PData d -> PData e) -> (a :-> b :-> c :-> d :-> e)
closure4 fS fL = closure3 (\a b c -> Clo_ (Id (a, b, c)) fS' fL') (\_ as bs cs -> PData $ clo_ PRDict (change PRDict liftedInstance) (WithPData (PData (Id (PData (as, bs, cs))))) fS' fL') where
    fS' (a, b, c) = fS a b c
    fL' s (PData (as, bs, cs)) = fL s as bs cs
    change :: PRDict x -> PRDict (PData x) -> PRDict (WithPData Id x)
    change PRDict PRDict = PRDict

type UseFunc f = forall a . PR a => a -> f a
type ApFunc f = forall a b . f (a :-> b) -> f a -> f b

useId :: UseFunc Id
useId = Id

apId :: ApFunc Id
apId (Id (Clo_ (Id e) fS _)) (Id x) = Id (fS e x)

usePData :: Int -> UseFunc PData
usePData = unsafeReplicatePData

apPData :: Int -> ApFunc PData
apPData s (PData (Clo_ (WithPData (PData (Id es))) _ fL)) xs = fL s es xs

vectorize1 :: (forall f . UseFunc f -> ApFunc f -> f a -> f b) -> (a :-> b)
vectorize1 f = closure1 fS fL where
    fS a = unId (f useId apId (Id a))
    fL s = f (usePData s) (apPData s)

vectorize2 :: (PR a) => (forall f . UseFunc f -> ApFunc f -> f a -> f b -> f c) -> (a :-> b :-> c)
vectorize2 f = closure2 fS fL where
    fS a b = unId (f useId apId (Id a) (Id b))
    fL s = f (usePData s) (apPData s)

vectorize3 :: (PR a, PR b) => (forall f . UseFunc f -> ApFunc f -> f a -> f b -> f c -> f d) -> (a :-> b :-> c :-> d)
vectorize3 f = closure3 fS fL where
    fS a b c = unId (f useId apId (Id a) (Id b) (Id c))
    fL s = f (usePData s) (apPData s)

vectorize4 :: (PR a, PR b, PR c) => (forall f . UseFunc f -> ApFunc f -> f a -> f b -> f c -> f d -> f e) -> (a :-> b :-> c :-> d :-> e)
vectorize4 f = closure4 fS fL where
    fS a b c d = unId (f useId apId (Id a) (Id b) (Id c) (Id d))
    fL s = f (usePData s) (apPData s)

-------------------------- Combinators -----------------------

apP :: PArray (a :-> b) :-> PArray a :-> PArray b
apP = closure2 apS apL where
    apS :: PArray (a :-> b) -> PArray a -> PArray b
    apS (PArray sF (PData (Clo_ (WithPData (PData (Id es))) _ fL))) (PArray sA as) = PArray s' (fL s' es as) where
        s' = min sF sA
    apL = undefined -- TODO: implement
