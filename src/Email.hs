module Email where

import Data.Aeson (FromJSON(..), ToJSON(..), Value(..))
import Data.Aeson.Types (typeMismatch)
import Data.CaseInsensitive (CI, original)
import qualified Data.CaseInsensitive as CI
import Data.Text (Text, pack)
import Data.Text.Encoding (decodeUtf8)
import Database.Persist (PersistField(..), PersistValue(..))
import Test.QuickCheck (Arbitrary(..))

newtype Email =
    Email (CI Text)
    deriving (Show, Eq, Ord)

mk :: Text -> Email
mk = Email . CI.mk

instance FromJSON Email where
    parseJSON (String text) = pure $ mk text
    parseJSON invalid = typeMismatch "Email" invalid

instance ToJSON Email where
    toJSON (Email ciText) = String . original $ ciText

instance PersistField Email where
    toPersistValue (Email ciText) = PersistText $ original ciText
    fromPersistValue (PersistDbSpecific bytestring) =
        Right . Email . CI.mk $ decodeUtf8 bytestring
    fromPersistValue _ = Left "HALP!"

instance Arbitrary Email where
    arbitrary = arbitrary >>= return . mk . pack
