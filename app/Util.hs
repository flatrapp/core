{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

module Util
    ( JsonError(..)
    , eitherJsonBody
    , emptyResponse
    , errorJson
    , maybeToEither
    , maybeTuple
    , showText
    )
where

import           Control.Arrow             ((***))
import           Control.Monad.IO.Class    (MonadIO)
import           Data.Aeson                ( FromJSON
                                           , object
                                           , (.=)
                                           , eitherDecodeStrict'
                                           )
import qualified Data.ByteString           as BS
import qualified Data.Text                 as T
import           Network.HTTP.Types.Status
import           Web.Spock

import qualified Model.CoreTypes           as CoreT

data JsonError
  = CredentialsWrong
  | Unauthorized
  | BadRequest String
  | NotFound
  | EmailNotVerified
  | InvitationCodeInvalid
  | VerificationCodeInvalid
  | NotInvited
  | UserEmailExists
  | InvitationEmailExists
  | UserDisabled
  deriving (Show)


errorJson :: Control.Monad.IO.Class.MonadIO m =>
             JsonError -> ActionCtxT ctx m b
errorJson err = do
  setStatus status
  json $
    object
    [ "error" .= object [
        "code" .= code,
        "message" .= msg
      ]
    ]
  where
    (code, msg) = (T.pack *** T.pack) strs
    (status, strs) = conv' err

    conv' :: JsonError -> (Status, (String, String))
    conv' CredentialsWrong        = (unauthorized401, ("credentials_wrong", "User does not exist or password is wrong."))
    conv' Unauthorized            = (unauthorized401, ("unauthorized", "You are not authorized to access this resource."))
    conv' (BadRequest errorMsg)   = (badRequest400,   ("bad_request", errorMsg))
    conv' NotFound                = (notFound404,     ("not_found", "The requested resource could not be found."))
    conv' EmailNotVerified        = (forbidden403,    ("email_not_verified", "You have not verified your email address yet."))
    conv' InvitationCodeInvalid   = (forbidden403,    ("invitation_code_invalid", "This is not a valid invitation code."))
    conv' VerificationCodeInvalid = (forbidden403,    ("verification_code_invalid", "Your email address could not be verified with this code."))
    conv' NotInvited              = (forbidden403,    ("not_invited", "Your email address is not invited."))
    conv' UserEmailExists         = (conflict409,     ("user_email_exists", "A user with this email address already exists."))
    conv' InvitationEmailExists   = (conflict409,     ("invitation_email_exists", "An invitation with this email address already exists."))
    conv' UserDisabled            = (forbidden403,    ("user_disabled", "This user is disabled and has to be enabled before being able to log in."))

maybeToEither :: a -> Maybe b -> Either a b
maybeToEither = flip maybe Right . Left

maybeTuple :: Maybe a -> Maybe b -> Maybe (a, b)
maybeTuple Nothing _         = Nothing
maybeTuple _ Nothing         = Nothing
maybeTuple (Just a) (Just b) = Just (a, b)

showText :: (Show a) => a -> T.Text
showText = T.pack . show

emptyResponse :: CoreT.ApiAction ctx a
emptyResponse = do
  setStatus noContent204
  bytes BS.empty

eitherJsonBody :: (FromJSON a) => CoreT.ApiAction ctx a
eitherJsonBody = do
  b <- body
  case eitherDecodeStrict' b of  -- TODO mapLeft
    -- TODO DO NOT expose literal errors to the client
    -- it might include sensitive application details.
    -- Might require a change to the Aeson library.
    Left err ->
      errorJson . BadRequest $ "Failed to parse json: " ++ err
    Right val ->
      return val
