-- infinicat repeatedly reads some file and writes its output to stdout.
-- It could also be done by a bash script:
--
--   while cat $1; do :; done
--
-- but who could possibly remember that syntax?

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE InterruptibleFFI #-}

module Main where

import Control.Concurrent (threadDelay)
import Control.Exception (IOException, catch, mask, onException)
import Control.Monad (unless, when)
import Data.ByteString (ByteString, hPut)
import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as B
import Data.Word (Word8)
import Foreign.C
import Foreign.C.String (withCString)
import Foreign.Ptr
import System.Environment (getArgs)
import System.Exit (exitWith, ExitCode (ExitSuccess))
import System.IO (Handle, stdout)

-- AFAICT base offers neither
-- - a blocking but interruptible open call
-- - a way to make an FD from a CInt (there is mkHandleFromFD, but the FD type
--   is not exported).
-- So, we'll define our own file API such that
-- - it blocks on open but is interruptible
-- - it can read strict bytestrings

-- Must be able to ctrl+c out of a blocked open.
foreign import ccall interruptible
  "open_blocking" c_open_blocking :: CString -> IO CInt

foreign import ccall unsafe
  "close" c_close :: CInt -> IO ()

foreign import ccall interruptible
  "read" c_read :: CInt -> Ptr Word8 -> CInt -> IO CInt

get :: CInt -> Int -> IO ByteString
get fd n = B.createAndTrim n $ \ptr ->
  fmap fromIntegral (c_read fd ptr (fromIntegral n))

withBinaryFile :: FilePath -> (CInt -> IO t) -> IO t
withBinaryFile fp k = mask $ \restore -> withCString fp $ \cstr -> do
  h <- restore (c_open_blocking cstr)
  -- open will return -1 in case there was an error, which will happen in case
  -- of a ctrl+c (GHC sends a SIGPIPE to it).o
  -- Since this is the only way for the program to exit, we'll call it a
  -- success...
  when (h == -1) (exitWith ExitSuccess)
  t <- restore (k h) `onException` c_close h
  c_close h
  pure t

cat_forever :: Int -> FilePath -> IO ()
cat_forever buffer_size fp = do
  _ <- withBinaryFile fp $ \h -> copy buffer_size h stdout `catch`
    (\(io_err :: IOException) -> putStrLn (show io_err) >> threadDelay 1000000)
  cat_forever buffer_size fp

copy :: Int -> CInt -> Handle -> IO ()
copy n src dst = do
  bytes <- get src n
  unless (B.null bytes || (B.length bytes == 0)) $ do
    hPut dst bytes
    copy n src dst

main :: IO ()
main = do
  args <- getArgs
  case args of
    [fp] -> cat_forever 4096 fp
    [fp,bs] -> case reads bs of
      [(n, "")] -> cat_forever n fp
      _ -> usage
    _ -> usage

usage :: IO ()
usage = putStrLn $ mconcat
  [ "infinicat: repeatedly cat a file to stdout, forever.\n\n"
  , "Usage:\n"
  , "  infinicat file_path [buffer_size]"
  ]
