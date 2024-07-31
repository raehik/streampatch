module Main where

import Streampatch.Test
import System.IO qualified as IO

main :: IO ()
main = test2Handle IO.stdin IO.stdout
