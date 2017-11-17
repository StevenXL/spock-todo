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
import Model.CoreTypes (migrateAll)
import Network.Wai.Middleware.Static (addBase, staticPolicy)
import Web.Configuration.Database (connStr)
import Web.Configuration.Response (Api, customErrorHandler)
import qualified Web.Resources.People as People
import Web.Spock (file, get, middleware, root, runSpock, spock)
import Web.Spock.Config

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
    get root $ file "index.html" "front-end/build/index.html"
    People.resources
