{-# LANGUAGE ExistentialQuantification, TypeFamilies, FlexibleContexts, UndecidableInstances, TypeOperators, RankNTypes #-}

module Data.Array.PRepr.Closure where

import Data.Array.PRepr.Class
import Data.Array.PRepr.Tuple (Id(..))

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
