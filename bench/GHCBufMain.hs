module Main where

import System.IO
import System.Environment
import GHC.IO.Buffer
import Data.ByteString as BS

import Control.Monad

-- Copied from cat source code
bufsize = 1024*128

go handle bufPtr = do
  read <- hGetBuf handle bufPtr bufsize
  when (read > 0) $ do
    hPutBuf stdout bufPtr read
    go handle bufPtr

main = do
  file    <- fmap Prelude.head getArgs
  handle  <- openFile file ReadMode
  buf     <- newByteBuffer bufsize WriteBuffer

  withBuffer buf $ go handle