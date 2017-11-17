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

import Control.Monad.Logger (runStdoutLoggingT)
import Database.Persist.Postgresql hiding (delete, get)
import Model.CoreTypes
import Network.Wai.Middleware.Static (addBase, staticPolicy)
import qualified Web.Actions.People as People
import Web.Configuration.Database
import Web.Configuration.Response
import Web.Spock
import Web.Spock.Config

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
    get root $ file "Text" "front-end/build/index.html"
    get "people" People.index
    get ("people" <//> var) People.show
    put ("people" <//> var) People.update
    delete ("people" <//> var) People.destroy
    post "people" People.create
