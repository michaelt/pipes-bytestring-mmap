import System.IO
import System.Environment
import Data.ByteString.Lazy as BS

import Control.Monad

main :: IO ()
main = do
  file    <- fmap Prelude.head getArgs
  handle  <- openFile file ReadMode
  buf     <- BS.hGetContents handle
  hPut stdout buf