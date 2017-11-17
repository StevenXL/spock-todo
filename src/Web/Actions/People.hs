{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module Web.Actions.People where

import Control.Monad.IO.Class (MonadIO)
import Database.Persist (SelectOpt(Asc), selectList)
import Database.Persist.Sql (SqlBackend)
import Model.CoreTypes
import Web.Configuration.Database (runSQL)
import Web.Spock (ActionCtxT, HasSpock, SpockConn, json)

getPeople ::
       ( MonadIO m
       , HasSpock (ActionCtxT ctx m)
       , SpockConn (ActionCtxT ctx m) ~ SqlBackend
       )
    => ActionCtxT ctx m b
getPeople = do
    allPeople <- runSQL $ selectList [] [Asc PersonId]
    json allPeople
