module Streampatch.Linearize (linearize, linearizeNum) where

import Streampatch.Patch
import Control.Monad.Trans.State.Strict ( put, get, runState )

{- | Linearize a list of absolute patches.

On success, returns the linearized patchlist, and the final cursor (indicating
the total length of the patch result).

The output list of patches have exclusively positive (zero included) seeks.

* We take @'Ord' i@ because I can't think of a case where you wouldn't have an
  'Ord' for a seek type, or have one that you wouldn't want to use.
* We require an addition monoid for the seek type. That's awkward to request
  through a type class, so we take in a bunch of functions upfront.
* By generalizing on the sort function, we can also generalize on the list type.
* I don't like the pure state monad usage. But I don't see a better method. We
  shouldn't use a fold, because we're not doing a fold. It's a failable map with
  some temporary state threaded through.
* TODO add proper error handling
-}
linearize
    :: forall f a i chk
    .  (Ord i, Traversable f)
    => (i -> i -> i)        -- ^ seek addition
    -> (i -> i -> i)        -- ^ seek subtraction
    -> i -- ^ zero seek
    -> (forall x. (x -> x -> Ordering) -> f x -> f x) -- ^ list-like sort
    -> (a -> i)             -- ^ how to obtain the length of a patch data
    -> f (Patch AbsSeek i chk a) -- ^ input absolute patches
    -> Either () (f (Patch (RelSeek FwdCursor) i chk a), i)
    -- ^ output sequenced patches
linearize fSeekAdd fSeekSub seek0 fSortBy fGetLen ps =
    let (eps, i) = runState (traverseM go (fSortBy comparePatchSeeks ps)) seek0
     in case eps of Left () -> Left (); Right ps' -> Right (ps', i)
  where
    comparePatchSeeks pl pr = compare (patchSeek pl) (patchSeek pr)
    go (Patch a s chk) = do
        cursor <- get
        let skip = s `fSeekSub` cursor
        if skip < seek0 then do
            -- next absolute seek is before cursor: current patch overlaps prev
            pure $ Left ()
        else do
            let cursor' = cursor `fSeekAdd` skip `fSeekAdd` fGetLen a
                p' = Patch a skip chk
            put cursor'
            pure $ Right p'

-- | 'linearize' with a 'Num' seek.
linearizeNum
    :: forall f a i chk
    .  (Ord i, Num i, Traversable f)
    => (forall x. (x -> x -> Ordering) -> f x -> f x) -- ^ list-like sort
    -> (a -> i)             -- ^ how to obtain the length of a patch data
    -> f (Patch AbsSeek i chk a) -- ^ input absolute patches
    -> Either () (f (Patch (RelSeek FwdCursor) i chk a), i)
    -- ^ output sequenced patches
linearizeNum = linearize (+) (-) 0

-- lol. ty hw-kafka-client
traverseM
    :: (Traversable t, Applicative f, Monad m)
    => (a -> m (f b))
    -> t a
    -> m (f (t b))
traverseM f xs = sequenceA <$> traverse f xs
