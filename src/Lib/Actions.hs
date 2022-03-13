{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Lib.Actions
  ( Shell,
    Command,
    ProcessState,
    createProc,
    createCmd,
    (./),
    sh,
    zsh,
    fish,
    nu,
  )
where

import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Data.String (IsString)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.IO.Handle (Handle)
import Lib.Conversion (From, Into (into), from)
import System.Posix (ProcessID)
import System.Posix.Process (executeFile)
import System.Posix.Types (ProcessID)
import System.Process (CreateProcess (std_err, std_out), ProcessHandle, createProcess)
import qualified System.Process as Proc
import XMonad (MonadIO, X, spawn)
import qualified XMonad as XM

type Command = (Text, Bool)

-- type Shell = Command

-- type ShScript = (Shell, Text)

instance From Command Text where
  from (path, env) = (if env then "/usr/bin/env " else "") <> path

type ProcessState = (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle)

instance (MonadIO m) => From CreateProcess (m ProcessState) where
  from proc = liftIO $ createProcess proc

instance (MonadIO m, From f (m ProcessState)) => From (IO f) (m ProcessState) where
  from fIO = (from :: f -> m ProcessState) =<< liftIO fIO

createProc :: Text -> [Text] -> CreateProcess
createProc exe args = Proc.proc (T.unpack exe) (T.unpack <$> args)

-- createShell ::  txt -> txt -> CreateProcess
-- createShell shell cmd = createProc shell ["-c", cmd]

type Shell txt = (txt, [txt], Bool)

createCmd :: Shell Text -> Text -> CreateProcess
createCmd (sh, args, False) cmd = createProc sh $ args <> [cmd]
createCmd (sh, args, True) cmd = createCmd ("/usr/bin/env " <> sh, args, False) cmd

(./) :: Shell Text -> Text -> CreateProcess
s ./ c = createCmd s c

sh :: Shell Text
sh = ("sh", ["-c"], False)

zsh :: Shell Text
zsh = ("zsh", ["-c"], True)

fish :: Shell Text
fish = ("fish", ["-c"], True)

nu :: Shell Text
nu = ("nu", ["-c"], True)
