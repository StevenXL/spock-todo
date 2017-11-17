{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module Web.Actions.People where

import Data.Aeson (Value(String), (.=), object)
import Data.Maybe (fromMaybe)
import Database.Persist
       (Entity, SelectOpt(Asc), delete, get, insert, replace, selectList)
import Model.CoreTypes
import Network.HTTP.Types (status201, status404, status406)
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
        mPerson <- runSQL $ get personId :: ApiAction (Maybe Person)
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
    mPerson <- jsonBody :: ApiAction (Maybe Person)
    case mPerson of
        Nothing -> do
            setStatus status406
            errorJson $ fromMaybe unknownError (mkErrorCode 2)
        (Just person) -> do
            newId <- runSQL $ insert person
            json $ object ["result" .= String "success", "id" .= newId]
