{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module Web.Actions.People where

import Data.Maybe (fromMaybe)
import Database.Persist (Entity, SelectOpt(Asc), get, selectList)
import Model.CoreTypes
import Network.HTTP.Types (status404)
import Web.Configuration.Database (runSQL)
import Web.Configuration.ErrorCode (mkErrorCode, unknownError)
import Web.Configuration.Response (ApiAction, errorJson)
import Web.Spock (json, setStatus)

getPeople :: ApiAction ()
getPeople = do
    allPeople <-
        runSQL $ selectList [] [Asc PersonId] :: ApiAction [Entity Person]
    json allPeople

getPerson :: PersonId -> ApiAction ()
getPerson =
    \personId -> do
        mPerson <- runSQL $ get personId :: ApiAction (Maybe Person)
        case mPerson of
            Nothing -> do
                setStatus status404
                errorJson $ fromMaybe unknownError (mkErrorCode 2)
            Just thePerson -> json thePerson
