{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lib
  ( Workspaces,
    getXDG,
    getXState,
    getXConfig,
    getXData,
    getXCache,
    ensureDir,
    ensureXDG,
    ensureXConfig,
  )
where

import Data.Functor (($>))
import Data.String (IsString)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.IO.Handle (Handle)
import Lib.Actions (sh)
import Lib.Actions.Pipes ((&>))
import qualified System.Directory as Dir
import qualified System.Environment as Env
import System.IO (IOMode (AppendMode, ReadMode, ReadWriteMode, WriteMode), openFile)
import System.Process (createProcess)

type Workspaces = [String]

ensureDir :: FilePath -> IO FilePath
ensureDir path = Dir.createDirectoryIfMissing True path $> path

getXDG :: Dir.XdgDirectory -> Text -> IO FilePath
getXDG dir name = Dir.getXdgDirectory dir $ T.unpack name

ensureXDG :: Dir.XdgDirectory -> Text -> IO FilePath
ensureXDG dir name = getXDG dir name >>= ensureDir

getXState :: Text -> IO FilePath
getXState name = Dir.getXdgDirectory Dir.XdgState (T.unpack name)

getXConfig :: Text -> IO FilePath
getXConfig name = Dir.getXdgDirectory Dir.XdgConfig (T.unpack name)

ensureXConfig :: Text -> IO FilePath
ensureXConfig name = getXConfig name >>= ensureDir

getXData :: Text -> IO FilePath
getXData name = Dir.getXdgDirectory Dir.XdgData (T.unpack name)

getXCache :: Text -> IO FilePath
getXCache name = Dir.getXdgDirectory Dir.XdgCache (T.unpack name)
