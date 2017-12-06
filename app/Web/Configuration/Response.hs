module Web.Configuration.Response where

import Data.Aeson (Value(String), (.=), object)
import Data.Text.Encoding (decodeUtf8)
import Database.Persist.Sql (SqlBackend)
import Network.HTTP.Types (Status(Status))
import Web.Configuration.ErrorCode
       (ErrorCode, errorCodeToCode, errorCodeToMsg)
import Web.Spock (ActionCtxT, SpockAction, SpockM, json)

type Api = SpockM SqlBackend () () ()

type ApiAction = SpockAction SqlBackend () ()

errorJson :: ErrorCode -> ApiAction ()
errorJson errorCode =
    json $
    object
        [ "result" .= String "failure"
        , "error" .=
          object
              [ "code" .= (errorCodeToCode errorCode)
              , "message" .= (errorCodeToMsg errorCode)
              ]
        ]

customErrorHandler :: Status -> ActionCtxT () IO ()
customErrorHandler (Status statusCode statusMsg) =
    json $
    object ["status" .= statusCode, "message" .= String (decodeUtf8 statusMsg)]
