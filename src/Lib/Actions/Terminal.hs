module Lib.Actions.Terminal
  ( Terminal,
    terminal,
  )
where

import Data.Text (Text)
import qualified Data.Text as T
import Lib.Actions

type Terminal = ShScript

terminal (shCmd :: Text) = (sh, shCmd)
terminal (shCmd :: Text) (args :: [Text]) = (sh, T.intercalate " " $ [shCmd] <> args)

executeTerm :: Terminal -> Maybe [(String, String)] -> IO a
executeTerm term = executeSh term

spawnTermPID :: MonadIO m => Terminal -> m ProcessID
spawnTermPID term = spawnShPID term

spawnTerm :: MonadIO m => Terminal -> m ()
spawnTerm term = spawnTermPID >> return ()
