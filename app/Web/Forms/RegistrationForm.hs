module Web.Forms.RegistrationForm where

import Control.Monad ((>=>))
import Data.Aeson (FromJSON, ToJSON)
import Data.String (IsString)
import Data.Text (Text, null, unpack)
import Database.Persist (getBy)
import qualified Email
import GHC.Generics (Generic)
import Model.CoreTypes (Unique(UniquePersonEmail))
import Text.Digestive
       (Form, Result(Error, Success), (.:), check, text, validate,
        validateM)
import Text.Read (readMaybe)
import Web.Configuration.Database (runSQL)
import Web.Configuration.Response (ApiAction)

data RegistrationRequest = RegistrationRequest
    { name :: Text
    , age :: Int
    , email :: Text
    } deriving (Generic)

instance FromJSON RegistrationRequest

instance ToJSON RegistrationRequest

registerForm :: Form Text ApiAction RegistrationRequest
registerForm =
    pure RegistrationRequest <*> "name" .: validateName <*>
    "age" .:
    validate
        (notEmpty >=> integer >=> greaterThan 0 >=> lessThanOrEq 100)
        (text Nothing) <*>
    "email" .: validateEmail
  where
    nonEmptyText = check "Cannot be empty" (not . Data.Text.null) $ text Nothing
    validateName = nonEmptyText
    validateEmail = validateM getPersonByEmail nonEmptyText

getPersonByEmail :: Text -> ApiAction (Result Text Text)
getPersonByEmail email' = do
    mPerson <- runSQL $ getBy (UniquePersonEmail $ Email.mk email')
    case mPerson of
        Nothing -> return $ Success email'
        Just _ -> return $ Error "Email is not available"

notEmpty :: IsString v => Text -> Result v Text
notEmpty input =
    if (not $ Data.Text.null input)
        then Success input
        else Error "Cannot be empty"

integer :: Text -> Result Text Int
integer input =
    maybe (Error "Unable to parse int") Success (textToMaybeInt input)
  where
    textToMaybeInt :: Text -> Maybe Int
    textToMaybeInt text' =
        fmap round (readMaybe . unpack $ text' :: Maybe Double)

greaterThan :: (Num a, Ord a, Show a) => a -> a -> Result Text a
greaterThan base other =
    if other > base
        then Success other
        else Error "Not big enough"

lessThanOrEq :: (Num a, Ord a, Show a) => a -> a -> Result Text a
lessThanOrEq base other =
    if other <= base
        then Success other
        else Error "Not small or equal enough"
