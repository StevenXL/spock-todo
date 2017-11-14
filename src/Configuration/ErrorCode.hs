module Configuration.ErrorCode
    ( ErrorCode
    , mkErrorCode
    , errorCodeToCode
    , errorCodeToMsg
    , unknownError
    ) where

type Msg = String

type Code = Int

data ErrorCode =
    ErrorCode Code
              Msg

mkErrorCode :: Int -> Maybe ErrorCode
mkErrorCode 1 = Just $ ErrorCode 1 "Failed to parse request body"
mkErrorCode 2 = Just $ ErrorCode 2 "Could not find resource"
mkErrorCode _ = Nothing

errorCodeToCode :: ErrorCode -> Code
errorCodeToCode (ErrorCode code _) = code

errorCodeToMsg :: ErrorCode -> Msg
errorCodeToMsg (ErrorCode _ msg) = msg

unknownError :: ErrorCode
unknownError = ErrorCode 99 "Unknown error"
