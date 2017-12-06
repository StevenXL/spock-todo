{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Model.CoreTypes where

import Data.Text (Text)
import Database.Persist.TH
import Email (Email)

share
    [mkPersist sqlSettings, mkMigrate "migrateAll"]
    [persistLowerCase|
Person json
  name Text
  age Int
  email Email sqltype=citext
  UniquePersonEmail email
  deriving Show
|]
