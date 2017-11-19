{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Web.Actions.People where

import Data.Aeson
       (FromJSON, ToJSON, Value(String), (.=), object, toJSON)
import Data.Maybe (fromMaybe)
import Data.Text (Text, null, unpack)
import Database.Persist
       (Entity, SelectOpt(Asc), delete, get, getBy, insert, replace,
        selectList)
import GHC.Generics (Generic)
import Model.CoreTypes
import Network.HTTP.Types
       (status201, status400, status404, status406)
import Text.Digestive
       (Form, Result(Error, Success), (.:), check, text, validate,
        validateM)
import Text.Digestive.Aeson (digestJSON, jsonErrors)
import Text.Digestive.Util (readMaybe)
import Web.Configuration.Database (runSQL)
import Web.Configuration.ErrorCode (mkErrorCode, unknownError)
import Web.Configuration.Response (ApiAction, errorJson)
import Web.Spock (json, jsonBody, setStatus)

index :: ApiAction ()
index = do
    allPeople <-
        runSQL $ selectList [] [Asc PersonId] :: ApiAction [Entity Person]
    json allPeople

show :: PersonId -> ApiAction ()
show =
    \personId -> do
        mPerson <- (runSQL $ get personId) :: ApiAction (Maybe Person)
        case mPerson of
            Nothing -> do
                setStatus status404
                errorJson $ fromMaybe unknownError (mkErrorCode 2)
            Just thePerson -> json thePerson

update :: PersonId -> ApiAction ()
update =
    \personId -> do
        mExistingPerson <- runSQL $ get personId :: ApiAction (Maybe Person)
        mReqBody <- jsonBody :: ApiAction (Maybe Person)
        case (mExistingPerson, mReqBody) of
            (Nothing, _) -> do
                setStatus status404
                errorJson $ fromMaybe unknownError (mkErrorCode 2)
            (_, Nothing) -> do
                setStatus status406
                errorJson $ fromMaybe unknownError (mkErrorCode 1)
            (Just _, Just reqBody) -> do
                runSQL $ replace personId reqBody
                setStatus status201
                json $ object ["result" .= String "success", "id" .= personId]

destroy :: PersonId -> ApiAction ()
destroy =
    \personId -> do
        mExistingPerson <- runSQL $ get personId :: ApiAction (Maybe Person)
        case mExistingPerson of
            Nothing -> do
                setStatus status404
            Just _ -> do
                runSQL $ delete (personId :: Key Person)
                json $ object ["result" .= String "success"]

create :: ApiAction ()
create = do
    mRegistrationRequest <- jsonBody :: ApiAction (Maybe RegistrationRequest)
    case mRegistrationRequest of
        Nothing -> do
            setStatus status400
            errorJson $ fromMaybe unknownError (mkErrorCode 1)
        Just request -> do
            (view, mRegReq) <- digestJSON registerForm (toJSON request)
            case mRegReq of
                Nothing -> do
                    setStatus status400
                    json $ jsonErrors view
                Just RegistrationRequest {..} -> do
                    setStatus status201
                    personId <- runSQL $ insert (Person name age email)
                    json $
                        object ["result" .= String "success", "id" .= personId]

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
    "age" .: validate validateAge (text Nothing) <*>
    "email" .: validateEmail
  where
    nonEmptyText = check "Cannot be empty" (not . Data.Text.null) $ text Nothing
    validateName = nonEmptyText
    validateEmail = validateM getPersonByEmail nonEmptyText
    validateAge :: Text -> Result Text Int
    validateAge potentialInt =
        maybe
            (Error "Unable to parse age")
            Success
            (readMaybe (unpack potentialInt) :: Maybe Int)

getPersonByEmail :: Text -> ApiAction (Result Text Text)
getPersonByEmail email' = do
    mPerson :: Maybe (Entity Person) <-
        runSQL $ getBy (UniquePersonEmail email')
    case mPerson of
        Nothing -> return $ Success "Email is available"
        Just _ -> return $ Error "Email is not available"
