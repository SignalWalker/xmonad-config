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
    (@>),
    (%>),
    (&>),
    (<@),
    (<%),
    (<&),
    devNull,
    writeNull,
    readNull,
    syscat,
  )
where

import Control.Exception (TypeError (TypeError), assert)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Text (Text)
import qualified Data.Text as T
import GHC.TypeLits (ErrorMessage (Text))
import Lib.Actions
import Lib.Conversion (From (from), Into (into))
import System.IO (Handle, IOMode (AppendMode, ReadMode, WriteMode), openFile)
import System.Process (CreateProcess (CreateProcess, std_err, std_in, std_out), StdStream (UseHandle), createPipe, createProcess)
import System.Process.Internals (CreateProcess)

devNull :: IOMode -> IO Handle
devNull = openFile "/dev/null"

writeNull :: IO Handle
writeNull = devNull WriteMode

readNull :: IO Handle
readNull = devNull ReadMode

syscat :: Text -> CreateProcess
syscat id = ("systemd-cat" :: Text) ./ ("--identifier=xmonad::" <> id)

class Pipe src snk res where
  pipe :: src -> snk -> res
  (|>) :: src -> snk -> res
  (<|) :: snk -> src -> res
  sr |> sn = pipe sr sn
  (<|) = flip (|>)

type MuxPipe p = (p, Bool, Bool)

class MuxPiper src snk res where
  (@>) :: src -> snk -> res
  (%>) :: src -> snk -> res
  (&>) :: src -> snk -> res
  (<@) :: snk -> src -> res
  (<%) :: snk -> src -> res
  (<&) :: snk -> src -> res
  (<@) = flip (@>)
  (<%) = flip (%>)
  (<&) = flip (&>)

instance (Pipe (MuxPipe src) snk res) => MuxPiper src snk res where
  sr @> sn = pipe (sr, True, False) sn
  sr %> sn = pipe (sr, False, True) sn
  sr &> sn = pipe (sr, True, True) sn

instance Pipe (MuxPipe CreateProcess) Handle CreateProcess where
  pipe (sr@(CreateProcess {std_out = sout, std_err = serr}), pout, perr) hdl =
    assert (pout || perr) $
      sr {std_out = if pout then UseHandle hdl else sout, std_err = if perr then UseHandle hdl else serr}

instance Pipe Handle CreateProcess CreateProcess where
  pipe hdl sn = sn {std_in = UseHandle hdl}

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
