{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

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
    getXDGUser',
    getXDGUser,
    getXDGUserOr,
  )
where

import Control.Exception (try)
import Data.Functor (($>))
import Data.String (IsString (fromString))
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Exception (Exception (displayException))
import GHC.IO.Handle (Handle)
import Lib.Actions (sh, (>$))
import Lib.Actions.Pipes ((|&>))
import qualified System.Directory as Dir
import qualified System.Environment as Env
import System.IO (IOMode (AppendMode, ReadMode, ReadWriteMode, WriteMode), openFile)
import System.Process (createPipe, createProcess, readProcess)
import System.Posix.Process (getProcessID)
import qualified XMonad.Hooks.ManageHelpers as XM
import XMonad (Query)

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

getXDGUser' :: Text -> IO (Either IOError Text)
getXDGUser' name = try $ T.strip . fromString <$> readProcess "xdg-user-dir" [T.unpack . T.toUpper . T.strip $ name] ""

getXDGUserOr :: Text -> Text -> IO Text
getXDGUserOr name dflt = do
  resE <- getXDGUser' name
  case resE of
    Left err -> do
      putStrLn ("getXDGUserOr '" <> T.unpack name <> "' IO Error: " <> displayException err)
      putStrLn $ " Using default: " <> T.unpack dflt
      return dflt
    Right res -> return res

eitherR :: (r -> t) -> Either l r -> Either l t
eitherR fn (Left l) = Left l
eitherR fn (Right r) = Right $ fn r

data XDGUserDir
  = Desktop
  | Documents
  | Download
  | Music
  | Pictures
  | PublicShare
  | Templates
  | Videos
  | Project
  | Source
  | Nix
  | Note

getXDGUser :: Text -> Maybe Text -> IO (Either IOError Text)
getXDGUser name further = eitherR (<> maybe "" ("/" <>) further) <$> getXDGUser' name

getXDGProject :: Text -> IO (Either IOError Text)
getXDGProject path = getXDGUser "PROJECT" $ Just path
