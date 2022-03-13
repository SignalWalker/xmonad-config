{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

module Lib.Actions.Pipes
  ( Pipe,
    (@>),
    (#>),
    (&>),
  )
where

import Control.Exception (TypeError (TypeError), assert)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Text (Text)
import qualified Data.Text as T
import GHC.TypeLits (ErrorMessage (Text))
import Lib.Actions
import Lib.Conversion (From (from), Into (into))
import System.IO (Handle, IOMode (AppendMode, WriteMode), openFile)
import System.Process (CreateProcess (CreateProcess, std_err, std_in, std_out), StdStream (UseHandle), createPipe, createProcess)
import System.Process.Internals (CreateProcess)

class Pipe src snk res where
  pipe :: src -> snk -> res
  (|>) :: src -> snk -> res
  sr |> sn = pipe sr sn

-- (@>) :: (Pipe (src, Bool, Bool) snk res) => src -> snk -> res
sr @> sn = pipe (sr, True, False) sn

-- (#>) :: (Pipe (src, Bool, Bool) snk res) => src -> snk -> res
sr #> sn = pipe (sr, False, True) sn

-- (&>) :: (Pipe (src, Bool, Bool) snk res) => src -> snk -> res
sr &> sn = pipe (sr, True, True) sn

-- class PipeSource src where
--   (@>) :: (Pipe src snk res) => src -> snk -> res
--   (#>) :: (Pipe src snk res) => src -> snk -> res
--   (&>) :: (Pipe src snk res) => src -> snk -> res
--   sr @> sn = pipe sr sn
--   sr #> sn = pipe sr sn
--   sr &> sn = pipe sr sn

-- instance (Pipe src snk) => Pipe src (IO snk) where
--   type Res src (IO snk) = IO (Res src snk)
--   pipe sr snIO = snIO >>= pipe sr

-- type instance PipeRes (CreateProcess, Bool, Bool) CreateProcess = (IO [CreateProcess])

type MuxPipe p = (p, Bool, Bool)

instance Pipe (MuxPipe CreateProcess) CreateProcess (IO [CreateProcess]) where
  pipe (sr@(CreateProcess {std_out = sout, std_err = serr}), pout, perr) sn = assert (pout || perr) $ do
    (rd, wr) <- createPipe
    return [sr {std_out = if pout then UseHandle wr else sout, std_err = if perr then UseHandle wr else serr}, from $ sn {std_in = UseHandle rd}]

-- type instance PipeRes (CreateProcess, Bool, Bool) [CreateProcess] = (IO [CreateProcess])

instance Pipe (MuxPipe CreateProcess) [CreateProcess] (IO [CreateProcess]) where
  pipe sr (sn : procs) = (\(r : _) -> r : procs) <$> pipe sr sn

instance (Pipe (MuxPipe CreateProcess) snk (IO [CreateProcess])) => Pipe (MuxPipe [CreateProcess]) snk (IO [CreateProcess]) where
  pipe ([sr], pout, perr) sn = pipe (sr, pout, perr) sn
  pipe (prv : nxt, pout, perr) sn = (prv :) <$> pipe (nxt, pout, perr) sn

instance Pipe (MuxPipe CreateProcess) Handle (IO CreateProcess) where
  pipe (sr@(CreateProcess {std_out = sout, std_err = serr}), pout, perr) hdl = assert (pout || perr) $ do
    return $ sr {std_out = if pout then UseHandle hdl else sout, std_err = if perr then UseHandle hdl else serr}

instance Pipe (MuxPipe CreateProcess) (IO Handle) (IO CreateProcess) where
  pipe sr hdlIO = hdlIO >>= pipe sr

instance Pipe (MuxPipe CreateProcess) (IO FilePath, IOMode) (IO CreateProcess) where
  pipe sr (pathIO, mode) = pathIO >>= \path -> pipe sr $ openFile path mode

-- instance (Pipe CreateProcess snk (IO [CreateProcess])) => Pipe [CreateProcess] snk (IO [CreateProcess]) where
--   pipe [sr] args = pipe sr args
--   pipe (prv : nxt) args = (prv :) <$> pipe nxt args

-- instance (Into (IO StdStream) toStrm) => Pipe CreateProcess toStrm where
--   type Res ()
--   pipe sr@(CreateProcess {std_out = sout, std_err = serr}) (toStrm, pout, perr) = do
--     strm <- into toStrm
--     return $ sr {std_out = if pout then strm else sout, std_err = if perr then strm else serr}

-- pipe sr strm@((_ :: Text, mode :: IOMode), _ :: Bool, _ :: Bool) = assert (mode == WriteMode || mode == AppendMode) $ pipe sr strm

-- instance (Into String toStr) => From (toStr, IOMode) (IO Handle) where
--   from (path, mode) = openFile (into path) mode

-- instance (Into String toStr) => From (IO toStr, IOMode) (IO Handle) where
--   from (pathIO, mode) = pathIO >>= \path -> from (path, mode)

-- instance From Handle StdStream where
--   from = UseHandle

-- instance (Into (IO Handle) toHdl) => From toHdl (IO StdStream) where
--   from toHdl = into toHdl

-- instance (Into (IO StdStream) toStrm) => Pipe toStrm CreateProcess (IO CreateProcess) where
--   pipe toStrm (sn, _, _) = do
--     strm <- into toStrm
--     return $ sn {std_in = strm}
