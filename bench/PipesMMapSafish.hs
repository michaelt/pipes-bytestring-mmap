import Pipes.ByteString.MMap 
import Pipes
import qualified Pipes.ByteString as B
import Pipes.Safe
import qualified System.IO as IO
import System.Environment (getArgs)
main = do
   file    <- fmap Prelude.head getArgs
   runSafeT $ runEffect $ unsafeMMapFile file >-> B.stdout



         