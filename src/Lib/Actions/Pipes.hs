{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

module Lib.Actions.Pipes
  ( Pipe (pipe, (|>)),
    MuxPipe,
    MuxPiper ((||>), (|:>), (<||), (<:|), (|&>), (<&|)),
    -- SimplePipe (simplePipe, (|&>), (<&|)),
    devNull,
    writeNull,
    readNull,
    syscat,
    Processable (process),
    Forkable (fork),
    forkT,
  )
where

import Control.Exception (TypeError (TypeError), assert)
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.String (IsString (fromString))
import Data.Text (Text)
import qualified Data.Text as T
import GHC.TypeLits (ErrorMessage (Text))
import Lib.Actions
import Lib.Conversion (From (from), Into (into))
import System.IO (Handle, IOMode (AppendMode, ReadMode, WriteMode), openFile)
import System.Process (CreateProcess (CreateProcess, std_err, std_in, std_out), StdStream (Inherit, UseHandle), createPipe, createProcess)
import qualified System.Process as Proc
import System.Process.Internals (CreateProcess)

devNull :: IOMode -> IO Handle
devNull = openFile "/dev/null"

writeNull :: IO Handle
writeNull = devNull WriteMode

readNull :: IO Handle
readNull = devNull ReadMode

syscat :: Text -> CreateProcess
syscat id = ("systemd-cat" :: Text) ./ ("--identifier=xmonad::" <> id)

class Processable p res where
  process :: MonadIO m => p -> m res

class Forkable f where
  fork :: MonadIO m => f -> m ()

process' :: MonadIO m => CreateProcess -> m ProcessState
process' p = liftIO $ createProcess p

processMulti' :: MonadIO m => [CreateProcess] -> m [ProcessState]
processMulti' ps = sequence $ process' <$> ps

forkT :: MonadIO m => Text -> m ()
forkT cmd = fork $ sh ./ cmd

instance (Processable p res) => Processable (IO p) res where
  process pIO = liftIO pIO >>= process

instance Processable CreateProcess ProcessState where
  process p = liftIO $ do
    putStrLn $ "process: " <> show (Proc.cmdspec p)
    process' p

instance (Processable p ProcessState) => Processable [p] [ProcessState] where
  process ps = sequence $ process <$> ps

instance (Forkable f) => Forkable (IO f) where
  fork fIO = liftIO fIO >>= fork

instance Forkable CreateProcess where
  fork p =
    let spec = Proc.cmdspec p
        specStr = case spec of
          Proc.RawCommand path args -> path <> " " <> show args
          Proc.ShellCommand cmd -> cmd
        tag = fromString $ case spec of
          Proc.RawCommand path args -> path
          Proc.ShellCommand cmd -> cmd
     in liftIO $ do
          putStrLn $ "fork: " <> specStr
          (procDef :: CreateProcess) <- readNull |> p {Proc.new_session = True}
          let (iOut, iErr) = (Proc.std_out procDef == Inherit, Proc.std_err procDef == Inherit)
          (procs :: [CreateProcess]) <-
            if iOut || iErr
              then (procDef, iOut, iErr) |> syscat tag
              else return [procDef]
          void $ processMulti' procs

instance Forkable [CreateProcess] where
  fork [p] = fork p
  fork (p : ps) = process' p >> fork ps

-- instance (MonadIO m, Processable p res) => Processable [p] (m [res]) where
--   process ps = sequence $ process <$> ps

class Pipe src snk res where
  pipe :: src -> snk -> res
  (|>) :: src -> snk -> res
  (<|) :: snk -> src -> res
  sr |> sn = pipe sr sn
  (<|) = flip (|>)

type MuxPipe p = (p, Bool, Bool)

class MuxPiper src snk res where
  (||>) :: src -> snk -> res
  (|:>) :: src -> snk -> res
  (<||) :: snk -> src -> res
  (<:|) :: snk -> src -> res
  (<||) = flip (||>)
  (<:|) = flip (|:>)
  (|&>) :: src -> snk -> res
  (<&|) :: snk -> src -> res
  (<&|) = flip (|&>)

instance (Pipe (MuxPipe src) snk res) => MuxPiper src snk res where
  sr ||> sn = pipe (sr, True, False) sn
  sr |:> sn = pipe (sr, False, True) sn
  sr |&> sn = pipe (sr, True, True) sn

instance Pipe (MuxPipe CreateProcess) Handle CreateProcess where
  pipe (sr@(CreateProcess {std_out = sout, std_err = serr}), pout, perr) hdl =
    assert (pout || perr) $
      sr {std_out = if pout then UseHandle hdl else sout, std_err = if perr then UseHandle hdl else serr}

instance Pipe Handle CreateProcess CreateProcess where
  pipe hdl sn = sn {std_in = UseHandle hdl}

instance (Pipe src snk res) => Pipe (IO src) snk (IO res) where
  pipe srcIO snk = (`pipe` snk) <$> srcIO

instance (Pipe src snk res) => Pipe src (IO snk) (IO res) where
  pipe src snkIO = pipe src <$> snkIO

instance Pipe (MuxPipe CreateProcess) CreateProcess (IO [CreateProcess]) where
  pipe sr sn = do
    (rd, wr) <- createPipe
    return [rd |> sn, sr |> wr]

instance Pipe (MuxPipe CreateProcess) [CreateProcess] (IO [CreateProcess]) where
  pipe sr [sn] = pipe sr sn
  pipe sr (prv : nxt) = (prv :) <$> pipe sr nxt

instance (Pipe (MuxPipe CreateProcess) snk (IO [CreateProcess])) => Pipe (MuxPipe [CreateProcess]) snk (IO [CreateProcess]) where
  pipe (prv : nxt, pout, perr) sn = (<> nxt) <$> pipe (prv, pout, perr) sn

instance (Pipe src CreateProcess CreateProcess) => Pipe src [CreateProcess] [CreateProcess] where
  pipe src [sn] = [pipe src sn]
  pipe src (prv : nxt) = prv : pipe src nxt

-- class SimplePipe src snk res where
--   simplePipe :: src -> snk -> res
--   (|&>) :: src -> snk -> res
--   sr |&> sn = simplePipe sr sn
--   (<&|) :: snk -> src -> res
--   (<&|) = flip (|&>)

-- instance (Pipe (MuxPipe src) snk res) => SimplePipe src snk res where
--   simplePipe sr = pipe (sr, True, True)
