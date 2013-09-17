{-# LANGUAGE FlexibleContexts, TypeFamilies, UndecidableInstances #-}

module Data.Array.PRepr.Nested where

import Data.Array.PRepr.Class
import Data.Array.PRepr.Primitive ()
import Data.Array.PRepr.Tuple ()

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
