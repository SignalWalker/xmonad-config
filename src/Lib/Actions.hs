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
    CmdRunner (createCmd, (./)),
    Runnable (runProc),
    (>$),
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
import System.Process (CmdSpec (RawCommand, ShellCommand), CreateProcess (std_err, std_out), ProcessHandle, createProcess)
import qualified System.Process as Proc
import XMonad (MonadIO, X, spawn)
import qualified XMonad as XM

type Command = (Text, Bool)

type ProcessState = (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle)

class CmdRunner rnr args where
  createCmd :: rnr -> args -> CreateProcess
  (./) :: rnr -> args -> CreateProcess
  rn ./ arg = createCmd rn arg

instance CmdRunner String [String] where
  createCmd = Proc.proc

instance CmdRunner String String where
  createCmd exe args = createCmd exe [args]

instance CmdRunner Text [Text] where
  createCmd exe args = createCmd (T.unpack exe) (T.unpack <$> args)

instance CmdRunner Text Text where
  createCmd exe cmd = createCmd exe [cmd]

type Shell txt = (txt, [txt])

instance CmdRunner (Shell Text) String where
  createCmd (sh, shArgs) args = createCmd (T.unpack sh) $ (T.unpack <$> shArgs) <> [args]

instance CmdRunner (Shell Text) Text where
  createCmd (sh, shArgs) args = createCmd sh $ shArgs <> [args]

(exe :: Text) >$ (args :: [Text]) = createCmd exe args

sh :: Shell Text
sh = ("sh", ["-c"])

zsh :: Shell Text
zsh = ("zsh", ["-c"])

fish :: Shell Text
fish = ("fish", ["-c"])

nu :: Shell Text
nu = ("nu", ["-c"])

class Runnable rn where
  runProc :: rn -> CreateProcess

instance Runnable CmdSpec where
  runProc (ShellCommand cmd) = sh ./ cmd
  runProc (RawCommand path args) = createCmd path args
