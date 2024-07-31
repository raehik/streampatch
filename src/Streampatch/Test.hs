{-# LANGUAGE OverloadedStrings #-}

module Streampatch.Test where

import Streampatch.Patch ( Patch(..) )
import Streampatch.Stream qualified as BP
import Streampatch.Check
import Data.ByteString qualified as B
import System.IO qualified as IO
import Bluefin.Eff

testFile :: FilePath -> IO ()
testFile fp = IO.withFile fp IO.ReadWriteMode testHandle

testHandle :: IO.Handle -> IO ()
testHandle hdl = runEff $ \io -> BP.runFwdInplaceStreamHandle io hdl
    (BP.patchFwdInplaceStream
        (fromIntegral . B.length)
        (checkMaxLen B.length (checkNulltermStrip fChk))
        ps
    )
  where
    ps = [Patch "long\0" 0 (MaxLen (Just 5) (NulltermStrip (Just 3) "123"))]
    fChk sBs chkBs = if sBs == chkBs then Nothing else Just "not equal"

test2Handle :: IO.Handle -> IO.Handle -> IO ()
test2Handle hdlIn hdlOut = runEff $ \io ->
    BP.runReadFromHandle io hdlIn $ \r ->
        BP.runWriteToHandle io hdlOut $ \w ->
            (BP.patchFromTo
                B.length
                (\_ () -> Nothing) -- (checkMaxLen B.length (checkNulltermStrip fChk))
                ps
                r w
            )
  where
    --ps = [Patch "long\0" 0 (MaxLen (Just 5) (NulltermStrip Nothing "123"))]
    ps = [Patch "long\0" 0 ()]
    fChk sBs chkBs = if sBs == chkBs then Nothing else Just "not equal"
