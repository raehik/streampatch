module Streampatch.Check where

import Data.ByteString qualified as B
import Data.ByteString ( ByteString )

{- Checks I designed (at least partially) in bytepatch:
* equality (prefix and exact)
* size (length?)
* digest
-}

{- | Check the target data isn't longer than a given value.

Really, we should check the patch data length. But this is an easy cheat.

Should be on the outside of all checks, since we should do it first.
-}
data MaxLen i chk = MaxLen
  { maxLen :: Maybe i

  -- | Wrapped check.
  , maxLenChk :: chk
  }

checkMaxLen
    :: Ord i
    => (a -> i)
    -> (a -> chk -> Maybe String)
    -> a -> MaxLen i chk -> Maybe String
checkMaxLen fLen fChk a (MaxLen mi chk) =
    case mi of
      Nothing -> fChk a chk
      Just i  -> if fLen a <= i then fChk a chk else Just "TODO too long"

{- | Check wrapped with a nullterm strip. Useful when you're patching data in
     executables.

Note that this check is not able to validate safe patching. You probably do not
want to overwrite the last null byte in a null-terminated null-padded string,
but we can't detect this here. Instead, make sure you're patching strings in
with their own null terminator, or add a "maximum size" check. (Better, both!)
-}
data NulltermStrip chk = NulltermStrip
  -- | Length of the check value. Past here, all bytes should be null.
  { nulltermStripLen   :: Maybe Int

  -- | Wrapped check to perform on the nullterm-stripped check value.
  , nulltermStripCheck :: chk
  }

checkNulltermStrip
    :: (ByteString -> chk -> Maybe String)
    -> ByteString -> NulltermStrip chk -> Maybe String
checkNulltermStrip fChk bs (NulltermStrip mi chk) =
    case mi of
      Nothing -> fChk bs chk
      Just i  ->
        let (bs', bsNulls) = B.splitAt i bs
        in  if   bsNulls == B.replicate (B.length bsNulls) 0x00
            then fChk bs' chk
            else Just "TODO ErrorBinUnexpectedNonNull bs'"

-- Not defining digest check here because it would pull in lots of packages.
