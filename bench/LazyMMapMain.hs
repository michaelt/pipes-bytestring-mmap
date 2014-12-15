module Main where

import System.IO (stdout)
import System.Environment
import System.IO.Posix.MMap.Lazy
import Data.ByteString.Lazy (hPut)

import Control.Monad

main :: IO ()
main = do
  file    <- fmap Prelude.head getArgs
  buf     <- unsafeMMapFile file
  hPut stdout buf