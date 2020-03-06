{-# LANGUAGE GADTs #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Exception (IOException, try)
import Control.Monad (forM_, void)
import Control.Monad.Trans.Class (lift)
import Data.ByteString.Lazy (hGet, hPut)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import Data.Word (Word32)
import qualified System.Console.Haskeline as Haskeline
import System.IO (IOMode (..), Handle, hClose, hIsEOF, openBinaryFile, withBinaryFile)

import qualified Debug.Trace as Debug

-- This is what it's all about. The Haskeline stuff that follows in this file
-- is just a wrapper to deliver file paths to this function.
--
-- The first list is the sources. They are concatenated in order and written
-- to each of the destinations in the second list (in order).
commit :: [(FilePath, Resource, BufferSize)] -> [(FilePath, Resource, WriteMode)] -> IO ()
commit sources sinks = do
  -- Open all sinks.
  with_handles (with_write_mode sinks) $ \sink_handles ->
    -- For each source: open it (if lazy), write it to every sink, then close
    -- it (if lazy).
    forM_ sources $ \(fp, source, buffer_size) -> with_handle ReadMode (fp, source) $ \source_h ->
      forM_ sink_handles $ \(sink_fp, sink_h) -> do
        Debug.traceM $ mconcat ["copying ", show fp, " -> ", show sink_fp]
        copy (fromIntegral buffer_size) source_h sink_h
  where
  with_write_mode = fmap $ \(fp, res, mode) -> case mode of
    Append -> (fp, res, AppendMode)
    Overwrite -> (fp, res, WriteMode)

copy :: Int -> Handle -> Handle -> IO ()
copy n src dst = do
  eof <- hIsEOF src
  case eof of
    True -> pure ()
    False -> do
      bytes <- hGet src n
      hPut dst bytes
      copy n src dst

with_handle :: IOMode -> (FilePath, Resource) -> (Handle -> IO t) -> IO t
with_handle mode (fp, res) k = case res of
  LazyResource     -> withBinaryFile fp mode k
  StrictResource h -> k h

with_handles :: [(FilePath, Resource, IOMode)] -> ([(FilePath, Handle)] -> IO t) -> IO t
with_handles = go []
  where
  go acc []                   k = k (reverse acc)
  go acc ((fp, res, mode):rs) k = with_handle mode (fp, res) $ \h -> go ((fp, h):acc) rs k

data Resource where
  LazyResource   :: Resource
  StrictResource :: !Handle -> Resource

data Target where
  Source :: !Resource -> !BufferSize -> Target
  Sink   :: !Resource -> !WriteMode  -> Target

print_target :: Target -> String
print_target (Source resource size) = mconcat ["source {", strictness_string, " ", show size, "}"]
  where
  strictness_string = case resource of
    LazyResource -> "lazy"
    StrictResource _ -> "strict"
print_target (Sink resource mode) = mconcat ["sink {", strictness_string, " ", mode_string, "}"]
  where
  mode_string = case mode of
    Append -> "append"
    Overwrite -> "overwrite"
  strictness_string = case resource of
    LazyResource -> "lazy"
    StrictResource _ -> "strict"

target_resource :: Target -> Resource
target_resource (Source r _) = r
target_resource (Sink   r _) = r

sources :: [(FilePath, Target)] -> [(FilePath, Resource, BufferSize)]
sources = mapMaybe pick_source

sinks :: [(FilePath, Target)] -> [(FilePath, Resource, WriteMode)]
sinks = mapMaybe pick_sink

pick_source :: (t, Target) -> Maybe (t, Resource, BufferSize)
pick_source (_, Sink _ _)   = Nothing
pick_source (t, Source r b) = Just (t, r, b)

pick_sink :: (t, Target) -> Maybe (t, Resource, WriteMode)
pick_sink (_, Source _ _) = Nothing
pick_sink (t, Sink r m)   = Just (t, r, m)

type BufferSize = Word32

data WriteMode = Overwrite | Append

type State = Map FilePath Target

initial_state :: State
initial_state = Map.empty

-- | Parameter to the "add" command. This determines whether it's a sink or
-- a source, and some config related to that.
data AddType where
  AddSink   :: !WriteMode  -> AddType
  AddSource :: !BufferSize -> AddType

-- | Strict targets are opened when they are added and closed when they are
-- removed. Lazy targets are opened and closed on-demand at commit time.
data Strictness = Strict | Lazy

data Command where
  Add    :: FilePath -> Strictness -> AddType -> Command
  Remove :: FilePath -> Command
  Commit :: Command
  Reset  :: Command
  Show   :: Command
  Help   :: Command
  Quit   :: Command

data RunResult where
  Continue :: Maybe Warning -> RunResult
  Stop     :: RunResult

data Warning where
  RemoveDoesNotExist :: Warning
  AddDuplicate       :: Warning
  CouldNotOpen       :: !IOException -> Warning
  CommitException    :: !IOException -> Warning
  deriving (Show)

-- It's in InputT because of the Help and Show commands.
-- FIXME probably better to factor into
-- - State changing and file I/O (add, remove, commit, reset)
-- - "meta" commands like show, help, and quit
run_command :: Command -> State -> Haskeline.InputT IO (RunResult, State)

run_command (Remove fp) = \st ->
  case Map.updateLookupWithKey (\_ _ -> Nothing) fp st of
    (Nothing, st')     -> pure (Continue (Just RemoveDoesNotExist), st')
    (Just target, st') -> do
      case target_resource target of
        LazyResource     -> pure ()
        StrictResource h -> lift $ hClose h
      pure (Continue Nothing, st')

run_command (Add fp strictness addType) = \st -> case Map.member fp st of
  True  -> pure (Continue (Just AddDuplicate), st)
  False -> do
    let mode = case addType of
          AddSink Append -> AppendMode
          AddSink Overwrite -> WriteMode
          AddSource _ -> ReadMode
    -- NB: we don't have to worry about bracketing and exception handling
    -- here, because if any async exception comes up, the whole program will
    -- die.
    m_resource <- case strictness of
      Lazy   -> pure (Right LazyResource)
      Strict -> lift $ try $ fmap StrictResource $ openBinaryFile fp mode
    case m_resource of
      Left  err      -> pure (Continue (Just (CouldNotOpen err)), st)
      Right resource ->
        let target = case addType of
              AddSink   m -> Sink resource m
              AddSource b -> Source resource b
        in  pure (Continue Nothing, Map.insert fp target st)

run_command Commit = \st -> do
  let targets_and_fps = Map.toList st
  outcome <- lift $ try (commit (sources targets_and_fps) (sinks targets_and_fps))
  case outcome of
    Left  err -> pure (Continue (Just (CommitException err)), st)
    Right _   -> pure (Continue Nothing, st)

run_command Reset = \st -> do
  forM_ (Map.elems st) $ \target -> case target_resource target of
    StrictResource h -> lift $ hClose h
    _ -> pure ()
  pure (Continue Nothing, initial_state)

-- TODO use a prettier printer
run_command Show = \st -> do
  forM_ (Map.toList st) $ \(fp, target) ->
    Haskeline.outputStrLn (mconcat [show fp, " : ", print_target target])
  pure (Continue Nothing, st)

run_command Quit = \st -> do
  forM_ (Map.elems st) $ \target -> case target_resource target of
    StrictResource h -> lift $ hClose h
    _ -> pure ()
  pure (Stop, st)

default_source_buffer_size :: BufferSize
default_source_buffer_size = 4096

default_sink_write_mode :: WriteMode
default_sink_write_mode = Append

parse_command :: String -> Either String Command
parse_command str = case words str of

  "add":strictness:addType:fp:rest -> do
    strictness' <- case strictness of
      "lazy"   -> Right Lazy
      "strict" -> Right Strict
      _ -> Left add_usage
    addType' <- case addType of
      "source" -> case rest of
        []  -> Right (AddSource default_source_buffer_size)
        [x] -> case reads x of
          [(n, "")] -> Right (AddSource n)
          _ -> Left add_source_usage
        _ -> Left add_source_usage
      "sink" -> case rest of
        [] -> Right (AddSink default_sink_write_mode)
        ["append"] -> Right (AddSink Append)
        ["overwrite"] -> Right (AddSink Overwrite)
        _ -> Left add_sink_usage
      _ -> Left add_sink_usage
    Right (Add fp strictness' addType')
    where
    add_usage        = "add {lazy|strict} {source|sink} path"
    add_source_usage = "add {lazy|strict} source path {buffer_size?}"
    add_sink_usage   = "add {lazy|strict} sink path {append|overwrite?}"

  "remove":fp:rest -> case rest of
    [] -> Right (Remove fp)
    _  -> Left "too many arguments to remove"

  "add":_ -> Left "add {sink|source} [path]"
  "remove":_ -> Left "remove {sink|source} [path]"

  "commit":xs -> if null xs then Right Commit else Left "commit takes no parameters"
  "reset":xs  -> if null xs then Right Reset  else Left "reset takes no parameters"
  "show":xs   -> if null xs then Right Show   else Left "show takes no parameters"
  "help":xs   -> if null xs then Right Help   else Left "help takes no parameters"
  "quit":xs   -> if null xs then Right Quit   else Left "quit takes no parameters"

  cmd:xs      -> Left ("Unknown command " ++ cmd)
  []          -> Left "No command given"

get_command :: Haskeline.InputT IO Command
get_command = do
  line <- get_nonempty_line
  case parse_command line of
    Right cmd -> pure cmd
    Left  err -> do
      Haskeline.outputStrLn err
      get_command
  where
  get_nonempty_line :: Haskeline.InputT IO String
  get_nonempty_line = do
    mLine <- Haskeline.handleInterrupt
      (Haskeline.outputStrLn "use the quit command to terminate the program" >> pure Nothing)
      (Haskeline.getInputLine "> ")
    case mLine of
      Nothing -> get_nonempty_line
      Just line -> pure line

main_interactive :: State -> Haskeline.InputT IO State
main_interactive !state = do
  command <- get_command
  mState <- run_command command state
  case mState of
    (Stop, state') -> pure state'
    (Continue m_warning, state') -> do
      case m_warning of
        Nothing -> pure ()
        Just warning -> Haskeline.outputStrLn (show warning)
      main_interactive state'

settings :: Haskeline.Settings IO
settings = Haskeline.Settings
  { Haskeline.complete = Haskeline.completeFilename
  , Haskeline.historyFile = Nothing
  , Haskeline.autoAddHistory = True
  }

main :: IO ()
main = Haskeline.runInputT settings
     -- On POSIX systems, this replaces the SIGINT handler for the duration of
     -- the main_interactive action.
     . Haskeline.withInterrupt
     . void
     $ main_interactive initial_state
