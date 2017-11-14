{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Configuration.Database
import Configuration.ErrorCode (mkErrorCode, unknownError)
import Configuration.Response
import Control.Monad.IO.Class
import Control.Monad.Logger (runStdoutLoggingT)
import Data.Aeson hiding (json)
import Data.Maybe (fromMaybe)
import qualified Data.Text.IO as T
import Database.Persist hiding (delete, get)
import qualified Database.Persist as P -- We'll be using P.get later for GET /people/<id>.
import Database.Persist.Postgresql hiding (delete, get)
import Model.CoreTypes
import Network.HTTP.Types
import Network.Wai.Middleware.Static (addBase, staticPolicy)
import Web.Spock
import Web.Spock.Config

-- Haskell in Depth Vitali ...
-- eitherDecode will give me the errors that failed
-- Errors use JsonPath;
-- Api represents hooks / handlers
-- ghcid -- checks if type-correct.
-- there is a difference between parsing json and validating json
-- Look for libraries on Hackage, not on Hoogle that's not what it's for
-- Look into default-extions in cabal file
-- jsonBody != jsonBody'
type Api = SpockM SqlBackend () () ()

main :: IO ()
main = do
    pool <- runStdoutLoggingT $ createPostgresqlPool connStr 1
    runSqlPool (runMigration migrateAll) pool
    intermediaryCfg <- defaultSpockCfg () (PCPool pool) ()
    let spockCfg = intermediaryCfg {spc_errorHandler = customErrorHandler}
    runSpock 8080 (spock spockCfg app)

app :: Api
app = do
    middleware $ staticPolicy (addBase "front-end/build/")
    get root $ do
        rootPage <- liftIO $ T.readFile "front-end/build/index.html"
        html rootPage
    get "people" $ do
        allPeople <- runSQL $ selectList [] [Asc PersonId]
        json allPeople
    get ("people" <//> var) $ \personId -> do
        maybePerson <- runSQL $ P.get personId :: ApiAction (Maybe Person)
        case maybePerson of
            Nothing -> do
                setStatus status404
                errorJson $ fromMaybe unknownError (mkErrorCode 2)
            Just thePerson -> json thePerson
    put ("people" <//> var) $ \personId -> do
        mExistingPerson <- runSQL $ P.get personId :: ApiAction (Maybe Person)
        mReqBody <- jsonBody :: ApiAction (Maybe Person)
        case (mExistingPerson, mReqBody) of
            (Nothing, _) -> do
                setStatus status404
                errorJson $ fromMaybe unknownError (mkErrorCode 2)
            (_, Nothing) -> do
                setStatus status406
                errorJson $ fromMaybe unknownError (mkErrorCode 1)
            (Just _, Just reqBody) -> do
                runSQL $ P.replace personId reqBody
                setStatus status201
                json $ object ["result" .= String "success", "id" .= personId]
    delete ("people" <//> var) $ \personId -> do
        mExistingPerson <- runSQL $ P.get personId :: ApiAction (Maybe Person)
        case mExistingPerson of
            Nothing -> do
                setStatus status404
            Just _ -> do
                runSQL $ P.delete (personId :: Key Person)
                json $ object ["result" .= String "success"]
    post "people" $ do
        maybePerson <- jsonBody :: ApiAction (Maybe Person)
        case maybePerson of
            Nothing -> do
                setStatus status406
                errorJson $ fromMaybe unknownError (mkErrorCode 2)
            (Just person) -> do
                newId <- runSQL $ insert person
                json $ object ["result" .= String "success", "id" .= newId]
