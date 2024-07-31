-- | Applying 'Patch's to streams.

module Streampatch.Stream where

import Streampatch.Patch
import Bluefin.Eff
import Bluefin.IO
import Bluefin.Compound
import System.IO qualified as IO
import Data.ByteString qualified as B
import Data.ByteString ( ByteString )

{- Patch targets:
* impure, handle (files, streams)
* MutableByteArray (Text)
* ForeignPtr (ByteString)
-}

data FwdInplaceStream e i a = MkFwdInplaceStream
  { readaheadImpl :: i -> Eff e a
  , overwriteImpl :: a -> Eff e ()
  , advanceImpl   :: i -> Eff e ()
  , getCursorImpl :: Eff e i -- for error messages
  }

readahead :: (e :> es) => FwdInplaceStream e i a -> i -> Eff es a
readahead s i = useImpl (readaheadImpl s i)

overwrite :: (e :> es) => FwdInplaceStream e i a -> a -> Eff es ()
overwrite s a = useImpl (overwriteImpl s a)

advance :: (e :> es) => FwdInplaceStream e i a -> i -> Eff es ()
advance s i = useImpl (advanceImpl s i)

getCursor :: (e :> es) => FwdInplaceStream e i a -> Eff es i
getCursor s = useImpl (getCursorImpl s)

-- | Patch a 'Handle' in-place. The 'Handle' must support (backwards) seeking.
runFwdInplaceStreamHandle
    :: forall eio es r
    .  (eio :> es)
    => IOE eio
    -> IO.Handle
    -> (forall e. FwdInplaceStream e Integer B.ByteString -> Eff (e :& es) r)
    -> Eff es r
runFwdInplaceStreamHandle io hdl k = useImplIn k MkFwdInplaceStream
  { readaheadImpl = \n -> effIO io $ do
      -- TODO any better way than doing this lol. kinda sucks
      bs <- B.hGet hdl (fromInteger n) -- TODO check for overflow here
      IO.hSeek hdl IO.RelativeSeek (-n)
      pure bs
  , overwriteImpl = \bs -> effIO io (B.hPut hdl bs)
  , advanceImpl = \n -> effIO io (IO.hSeek hdl IO.RelativeSeek n)
  , getCursorImpl = effIO io (IO.hTell hdl)
  }

{- for Text etc. problem: got to manually handle byte->@a@ (e.g.
   UTF-8 char) conversion. big pain. ignore for now
runFwdInplaceStreamMutArr
    :: forall a es r
    -> MutableByteArray
    -> Int -- ^ length
    -> (forall e. FwdInplaceStream e Int a -> Eff (e :& es) r)
    -> Eff es r
-}

{-
TODO does NOT check for extending the file. That is PERMITTED, if weird and
probably bad. We shouldn't check here, because we can obtain the total length of
the patched file "for free" while linearizing.
-}
patchFwdInplaceStream
    :: forall a i chk e es list
    .  (e :> es, Foldable list)
    => (a -> i)
    -> (a -> chk -> Maybe String) -- check
    -> list (Patch (RelSeek FwdCursor) i chk a)
    -> FwdInplaceStream e i a
    -> Eff es ()
patchFwdInplaceStream fLenA fCheck ps s = mapM_ go ps
  where
    go p = do
        advance s (patchSeek p)
        let pa = patchData p
        sa <- readahead s (fLenA pa)
        case fCheck sa (patchCheck p) of
          Nothing -> overwrite s pa
          Just e  -> error $ "TODO need error handling: " <> e

{- Copy:
* source needs to support reading and forward seeking. nothing else
* target needs to support writing. nothing else
* track cursor (getCursor) for nice error messages
  * ...can probably do this manually
-}

{- Examples:
* stdin -> stdout (handle, all done for us)
* file to file (handle, all done for us)
* ByteString to ByteString (we need to do all the buffering, cursors, errors)
  * this one is much more complex
  * better, ForeignPtr to ForeignPtr (and also a MutableByteArray version)
-}

data ReadFrom e a = MkReadFrom
  { readFromImpl         :: Int -> Eff e a
  , readFromUntilEndImpl ::        Eff e a
  }

readFrom :: (e :> es) => ReadFrom e a -> Int -> Eff es a
readFrom r i = useImpl (readFromImpl r i)

readFromUntilEnd :: (e :> es) => ReadFrom e a -> Eff es a
readFromUntilEnd r = useImpl (readFromUntilEndImpl r)

-- | The 'Handle' need not support seeking.
runReadFromHandle
    :: forall eio es r
    .  (eio :> es)
    => IOE eio
    -> IO.Handle
    -> (forall e. ReadFrom e ByteString -> Eff (e :& es) r)
    -> Eff es r
runReadFromHandle io hdl k = useImplIn k MkReadFrom
  { readFromImpl = \i -> effIO io (B.hGet hdl i)
  , readFromUntilEndImpl = effIO io (B.hGetContents hdl)
  }

data WriteTo e a = MkWriteTo { writeToImpl :: a -> Eff e () }
writeTo :: (e :> es) => WriteTo e a -> a -> Eff es ()
writeTo w a = useImpl (writeToImpl w a)

-- | The 'Handle' need not support seeking.
runWriteToHandle
    :: forall eio es r
    .  (eio :> es)
    => IOE eio
    -> IO.Handle
    -> (forall e. WriteTo e ByteString -> Eff (e :& es) r)
    -> Eff es r
runWriteToHandle io hdl k = useImplIn k MkWriteTo
  { writeToImpl = \bs -> effIO io (B.hPut hdl bs)
  }

{- | Patch between arbitrary streams by reading and writing.

Between patches, we copy the appropriate amount of data from the read stream to
the write stream.

/Do not pass the same stream for both the read and write stream./ This is not
checked, but will result in a (safe) mess. For this, use the in-place patcher.

Highly general, but potentially inefficient due to the multiple copies and
intermediate values.

Naturally slower than in-place patching. But better than copying then patching
in-place.
-}
patchFromTo
    :: (er :> es, ew :> es, Foldable list)
    => (a -> Int)
    -> (a -> chk -> Maybe String) -- check
    -> list (Patch (RelSeek FwdCursor) Int chk a)
    -> ReadFrom er a
    -> WriteTo  ew a
    -> Eff es ()
patchFromTo fLenA fCheck ps r w = do
    -- execute patchscript
    mapM_ go ps

    -- copy any remaining data
    as <- readFromUntilEnd r
    writeTo w as

  where
    go p = do
        -- copy up to next patch location
        asCopy <- readFrom r (patchSeek p)
        writeTo w asCopy

        -- read data to overwrite, write patch data if check succeeds
        let ap = patchData p
        as <- readFrom r (fLenA ap)
        case fCheck as (patchCheck p) of
          Nothing -> writeTo w ap
          Just e  -> error $ "TODO need error handling: " <> e
