-- infinicat repeatedly reads some file and writes its output to stdout.
-- It could also be done by a bash script:
--
--   while cat $1; do :; done
--
-- but who could possibly remember that syntax?

module Main where

import Data.ByteString.Lazy (hGet, hPut)
import System.Environment (getArgs)
import System.IO (Handle, IOMode (ReadMode), hIsEOF, stdout, withBinaryFile)

cat_forever :: Int -> FilePath -> IO x
cat_forever buffer_size fp = do
  _ <- withBinaryFile fp ReadMode $ \h -> copy buffer_size h stdout
  cat_forever buffer_size fp

copy :: Int -> Handle -> Handle -> IO ()
copy n src dst = do
  eof <- hIsEOF src
  case eof of
    True -> pure ()
    False -> do
      bytes <- hGet src n
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
