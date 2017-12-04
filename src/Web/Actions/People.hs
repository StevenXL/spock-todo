{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Web.Actions.People where

import Data.Aeson (Value(String), (.=), object, toJSON)
import Data.Maybe (fromMaybe)
import Database.Persist
       (Entity, SelectOpt(Asc), delete, get, insert, replace, selectList)
import Model.CoreTypes
import qualified Model.Email as Email
import Network.HTTP.Types
       (status201, status400, status404, status406)
import Text.Digestive.Aeson (digestJSON, jsonErrors)
import Web.Configuration.Database (runSQL)
import Web.Configuration.ErrorCode (mkErrorCode, unknownError)
import Web.Configuration.Response (ApiAction, errorJson)
import Web.Forms.RegistrationForm
       (RegistrationRequest(..), registerForm)
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
                    personId <-
                        runSQL $ insert (Person name age (Email.mk email))
                    json $
                        object ["result" .= String "success", "id" .= personId]
