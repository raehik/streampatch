module Streampatch.Patch where

import GHC.Generics ( Generic )

-- | A single patch of @a@ on a stream located with @i@.
data Patch (seek :: SeekType) i chk a = Patch
  { patchData :: a
  , patchSeek :: i

  -- | A check to perform on the existing data.
  , patchCheck :: chk
  } deriving stock (Generic, Show)

data SeekType
  -- | Seek defines an absolute position in a stream.
  = AbsSeek

  -- | Seek defines a position relative to a cursor.
  | RelSeek CursorType

data CursorType
  -- | Cursor may only move forwards. (Seeks may only be positive.)
  = FwdCursor

  -- | Cursor may move forwards and backwards.
  | FwdBackCursor
