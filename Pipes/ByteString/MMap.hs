{-# LANGUAGE CPP, BangPatterns, ForeignFunctionInterface #-}
--------------------------------------------------------------------
-- |
-- Module    :  System.IO.Posix.MMap
-- Copyright :  (c) Galois, Inc. 2007
-- License   :  BSD3
--
-- Maintainer:  Don Stewart <dons@galois.com>
-- Stability :  provisional
-- Portability: non-portable -- posix only
--
-- Lazy, chunk-wise memory mapping.
--
-- Memory map a file as a lazy ByteString. Finalisers are associated
-- cached-sized portions of the file, which will be deallocated as
-- those chunks go out of scope.
--
-- Unlike strict Bytestrings, mmapFile for Lazy ByteStrings will
-- deallocate chunks of the file.
--
-- The storage manager is used to free chunks of the mapped memory. When
-- the garbage collector notices there are no further references to 
-- a chunk, a call to munmap is made.
--
-- In effect, the file is mmapped once, lazily, then covered with finalizers
-- for each chunk. When any chunk goes out of scope, that part is
-- deallocated. We must allocate the spine of the structure strictly
-- though, to ensure finalizers are registered for the entire file.
--
-- The Haskell garbage collector decides when to run based on heap
-- pressure, however the mmap stores memory outside the Haskell heap, 
-- so those resources are not counted when deciding to run the garbage
-- collect. The result is that finalizers run less often than you might
-- expect, and it is possible to write a lazy bytestring mmap program 
-- that never deallocates (and thus doesn't run in constant space).
-- 'performGC' or 'finalizerForeignPtr' can be used to trigger collection
-- at sensible points.
--
-- Note: this operation may break referential transparency! If 
-- any other process on the system changes the file when it is mapped
-- into Haskell, the contents of your 'ByteString' will change.
--
module Pipes.ByteString.MMap (

      unsafeMMapFile -- :: FilePath -> IO ByteString
      , unsaferMMapFile
    ) where

import Pipes.ByteString.MMap.Internal

import Foreign.C.Types
import Foreign.Ptr
import Foreign.ForeignPtr.Safe (finalizeForeignPtr )
import Data.Word
import Data.ByteString
import Data.ByteString.Internal
import System.Posix
import Pipes
import Control.Exception
import qualified Pipes.Safe as Safe
import Pipes.Safe (MonadSafe)
import Control.Monad
--
-- | The 'unsafeMMapFile' and 'unsaferMMapFile' functions maps 
-- a file or device into memory as
-- a ByteString producer, made of 64*pagesize unmappable chunks of bytes.
--
-- The storage manager is used to free chunks that go out of scope,
-- and unlike strict mmapped bytestrings, a memory mapped byteString producer will
-- be deallocated in chunks (so you can write traversals that run in
-- constant space).
--
-- However, the size of the mmapped resource is not known by the Haskell
-- GC, it appears only as a small ForeignPtr. This means that the
-- Haskell GC may not not run as often as you'd like, leading to delays
-- in unmapping chunks.
-- 
-- Appropriate use of performGC or finalizerForeignPtr may be required
-- to ensure deallocation, as resources allocated by mmap are not
-- tracked by the Haskell garbage collector.
--
-- For example, when writing out a bytestring producer allocated with mmap,
-- you may wish to finalizeForeignPtr when each chunk is written, as the 
-- chunk goes out of scope, rather than relying on the garbage collector
-- to notice the chunk has gone.
--
-- This operation is unsafe: if the file is written to by any other
-- process on the system, the 'ByteString' contents will change in
-- Haskell.

unsafeMMapFile :: MonadSafe m => FilePath -> Producer ByteString m ()
unsafeMMapFile path = do
    fd   <- liftIO $ openFd path ReadOnly Nothing defaultFileFlags
    flip Safe.finally (liftIO $ closeFd fd) $ do
        stat <- liftIO $ getFdStatus fd
        let size = fromIntegral (fileSize stat)
        ptr  <- liftIO $ c_mmap size (fromIntegral fd)
        if ptr == nullPtr
          then error "Pipes.ByteString.MMap: unable to mmap file!"
          else chunks chunk_size ptr (fromIntegral size)

-- must be page aligned.
chunk_size :: CSize
chunk_size = 64 * fromIntegral pagesize -- empircally derived

unsaferMMapFile :: FilePath -> (Producer ByteString IO () -> IO ()) -> IO ()
unsaferMMapFile path op = do
    fd   <- openFd path ReadOnly Nothing defaultFileFlags
    flip finally (closeFd fd) $ do
        stat <- getFdStatus fd
        let size = fromIntegral (fileSize stat)
        ptr0  <- liftIO $ c_mmap size (fromIntegral fd)
        when (ptr0 == nullPtr) $ error "Pipes.ByteString.MMap: unable to mmap file!"
        let loop !ptr !rest
              | rest <= 0 = return ()
              | otherwise = let s     = min chunk_size rest
                                ptr'  = ptr `plusPtr` fromIntegral s
                                rest' = rest - s
                            in do c  <- liftIO $ unsafePackMMapPtr ptr s
                                  yield c
                                  loop ptr' rest' -- need to be strict
        op (loop ptr0 (fromIntegral size))

--
-- Break the file up into chunks.
-- Have separate munmap finalizers for each chunk.
--
chunks :: MonadSafe m => CSize -> Ptr Word8 -> CSize -> Producer ByteString m ()
chunks chunksize p bytes = loop p bytes
  where
    loop !ptr !rest
          | rest <= 0 = return ()
          | otherwise = let s     = min chunksize rest
                            ptr'  = ptr `plusPtr` fromIntegral s
                            rest' = rest - s
                        in do c  <- liftIO $ unsafePackMMapPtr ptr s
                              yield c
                              loop ptr' rest' -- need to be strict


foreign import ccall unsafe "unistd.h getpagesize"
    pagesize :: CInt

