{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module Model.Email where

import Data.Aeson
       (FromJSON(..), ToJSON(..), Value(..), (.:), withObject)
import Data.CaseInsensitive (CI, original)
import qualified Data.CaseInsensitive as CI
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import Database.Persist (PersistField(..), PersistValue(..))

newtype Email =
    Email (CI Text)
    deriving (Show)

mk :: Text -> Email
mk = Email . CI.mk

instance FromJSON Email where
    parseJSON =
        withObject "Email" $ \o -> o .: "email" >>= return . Email . CI.mk

instance ToJSON Email where
    toJSON (Email ciText) = String . original $ ciText

instance PersistField Email where
    toPersistValue (Email ciText) = PersistText $ original ciText
    fromPersistValue (PersistDbSpecific bytestring) =
        Right . Email . CI.mk $ decodeUtf8 bytestring
    fromPersistValue _ = Left "HALP!"
