module Main where

import System.IO
import System.Environment
import Data.ByteString as BS

import Control.Monad

-- Copied from cat source code
bufsize = 1024*128

go handle buf = do
  hPut stdout buf
  eof <- hIsEOF handle
  unless eof $ do
    buf <- hGetSome handle bufsize
    go handle buf

main = do
  file    <- fmap Prelude.head getArgs
  handle  <- openFile file ReadMode
  buf     <- hGetSome handle bufsize
  hSetBuffering stdin $ BlockBuffering (Just bufsize)
  hSetBuffering stdout $ BlockBuffering (Just bufsize)
  go handle buf