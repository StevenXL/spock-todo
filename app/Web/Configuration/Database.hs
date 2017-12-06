module Web.Configuration.Database where

import Control.Monad.Logger (LoggingT, runStdoutLoggingT)
import Database.Persist.Postgresql (ConnectionString, runSqlConn)
import Database.Persist.Sql (SqlBackend, SqlPersistT)
import Web.Spock (HasSpock, SpockConn, runQuery)

connStr :: ConnectionString
connStr =
    "host=localhost port=5432 user=postgres dbname=spocktest password=test"

runSQL ::
       (HasSpock m, SpockConn m ~ SqlBackend)
    => SqlPersistT (LoggingT IO) a
    -> m a
runSQL action = runQuery $ \conn -> runStdoutLoggingT $ runSqlConn action conn
