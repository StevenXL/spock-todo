module Configuration.Response where

import Data.Aeson (Value(String), (.=), object)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import Database.Persist.Sql (SqlBackend)
import Network.HTTP.Types (Status(Status))
import Web.Spock (ActionCtxT, SpockAction, json)

type ApiAction a = SpockAction SqlBackend () () a

errorJson :: Int -> Text -> ApiAction ()
errorJson code message =
    json $
    object
        [ "result" .= String "failure"
        , "error" .= object ["code" .= code, "message" .= message]
        ]

customErrorHandler :: Status -> ActionCtxT () IO ()
customErrorHandler (Status statusCode statusMsg) =
    json $
    object ["status" .= statusCode, "message" .= String (decodeUtf8 statusMsg)]
