{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lib
  ( Workspaces,
    logDir,
    getLogFile,
    openLog,
    writeLog,
    appendLog,
    readLog,
    readWriteLog,
    mainLogFile,
    envLogFile,
  )
where

import Data.String (IsString)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.IO.Handle (Handle)
import Lib.Actions (sh, (./))
import Lib.Actions.Pipes ((&>))
import qualified System.Directory as Dir
import qualified System.Environment as Env
import System.IO (IOMode (AppendMode, ReadMode, ReadWriteMode, WriteMode), openFile)
import System.Process (createProcess)

type Workspaces = [String]

logDir :: IO FilePath
logDir = Dir.getXdgDirectory Dir.XdgState "xmonad" >>= Dir.createDirectoryIfMissing True *> return

getLogFile :: String -> IO FilePath
getLogFile name = logDir >>= \ld -> return $ ld <> "/" <> name

openLog :: String -> IOMode -> IO Handle
openLog name mode = do
  path <- getLogFile name
  openFile path mode

writeLog :: String -> IO Handle
writeLog name = openLog name WriteMode

appendLog :: String -> IO Handle
appendLog name = openLog name AppendMode

readLog :: String -> IO Handle
readLog name = openLog name ReadMode

readWriteLog :: String -> IO Handle
readWriteLog name = openLog name ReadWriteMode

mainLogFile :: IO FilePath
mainLogFile = getLogFile "main.log"

envLogFile :: Text -> Text
envLogFile name = "${XDG_STATE_HOME:-$HOME/.local/state}/" <> name <> ".log"
