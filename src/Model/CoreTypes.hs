{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Model.CoreTypes where

import Data.Text (Text)
import Database.Persist.TH

share
    [mkPersist sqlSettings, mkMigrate "migrateAll"]
    [persistLowerCase|
Person json
  name Text
  age Int
  deriving Show
|]
