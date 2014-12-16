module Main where

import Pipes
import qualified Pipes.Prelude as PP
import qualified Pipes.ByteString as P
import System.IO
import System.Environment
import qualified Data.ByteString as B
bufsize :: Int
bufsize = 1024*1024

main = do
  file    <- fmap Prelude.head getArgs
  withFile file ReadMode $ \handle ->
     runEffect  (P.hGetSome bufsize handle >-> P.stdout)
