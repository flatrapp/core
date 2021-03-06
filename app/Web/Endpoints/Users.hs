{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Web.Endpoints.Users (routeUsers) where

import           Control.Monad.IO.Class       (liftIO)
import           Data.HVect                   (HVect, ListContains)
import           Data.Text                    (Text, append)
import           Database.Persist             hiding (delete, get)
import qualified Database.Persist             as P
import           Network.HTTP.Types.Status    (created201)
import           System.Random                (StdGen, getStdGen)
import           Web.Spock

import qualified Config                       as Cfg
import qualified Crypto
import qualified Mail
import           Model.CoreTypes              (ApiAction, Api, Email, apiCfg)
import qualified Model.SqlTypes               as SqlT
import qualified Model.JsonTypes.Registration as JsonRegistration
import qualified Model.JsonTypes.User         as JsonUser
import           Util                         ( errorJson, eitherJsonBody
                                              , JsonError(..))
import           Query.Util                   ( runSQL
                                              , trySqlSelectFirst
                                              , trySqlSelectFirstError
                                              , sqlAssertIsNotThere)
import           Web.Auth                     (getCurrentUser, authHook)

routeUsers :: Api (HVect xs)
routeUsers = do
  post "users" $
    eitherJsonBody >>= postUsersAction -- TODO use the Nothing case as intermediate action and chain it in somehow
  prehook authHook $ do
    get ("users" <//> "current") currentUserAction
    get "users" $ do
      mCode <- param "code"
      case mCode of
        Nothing -> getUsersAction
        Just code -> verifyEmailAction code -- TODO maybe simplify
    get ("users" <//> var) $ returnUserById . Just
    -- TOOD implement put "users" $ do
    -- TODO implement disable user

returnUserById :: Maybe (Key SqlT.User) -> ApiAction ctx m
returnUserById Nothing =
  -- TODO combine with registration... because this function is also called when there is no registration happening
  errorJson UserEmailExists
returnUserById (Just userId) =
  trySqlSelectFirst SqlT.UserId userId >>= json . JsonUser.jsonUser

getUsersAction :: ListContains n Email xs => ApiAction (HVect xs) a
getUsersAction =
  json =<< (map JsonUser.jsonUser <$> runSQL (selectList [] [Asc SqlT.UserId]))

-- |Endpoint to verify the email address of a user with a code that was sent to them via email
verifyEmailAction :: ListContains n Email xs => Text -> ApiAction (HVect xs) a
verifyEmailAction code = do
  (Entity userId _user) <- trySqlSelectFirstError VerificationCodeInvalid SqlT.UserVerifyCode $ Just code
  runSQL $ P.updateWhere [SqlT.UserId ==. userId]
                         [SqlT.UserVerifyCode =. Nothing]
  returnUserById $ Just userId

-- |Register a new user
-- If they provide an invitation code but not an email address they will not
-- need to verify it if the code is valid. If they provide an email address
-- they will be sent an email with a confirmation code.
-- If they provide neither the requests fails.
-- If they provide both, the code will be used  -- TODO make it fail aswell
postUsersAction :: JsonRegistration.Registration -> ApiAction ctx a
postUsersAction registration
  | Just code <- JsonRegistration.invitationCode registration = do
      -- fails if user provided code but code is not in DB
      (Entity invitationId theInvitation) <- trySqlSelectFirstError InvitationCodeInvalid SqlT.InvitationCode code
      let email = SqlT.invitationEmail theInvitation
      -- fails if user exists
      _user <- sqlAssertIsNotThere UserEmailExists SqlT.UserEmail email
      -- remove Invitation after it has been used
      runSQL $ P.delete invitationId
      setStatus created201
      gen <- liftIO getStdGen
      newId <- registerUser registration gen email Nothing
      returnUserById newId
  -- no invitationCode provided but email address
  | Just email <- JsonRegistration.email registration = do
      gen <- liftIO getStdGen
      cfg <- apiCfg <$> getState
      if email `elem` Cfg.whitelistedMails cfg then do
        setStatus created201
        newId <- registerUser registration gen email Nothing
        returnUserById newId
      else do
        -- fails if user provided email but is not invited
        _inv <- trySqlSelectFirstError NotInvited SqlT.InvitationEmail email
        -- fails if user exists
        _user <- sqlAssertIsNotThere UserEmailExists SqlT.UserEmail email
        let username = JsonRegistration.firstName registration
              `append` JsonRegistration.lastName registration
        verificationCode <- liftIO Crypto.verificationCode
        liftIO $ Mail.sendBuiltMail cfg email
                 $ Mail.buildVerificationMail verificationCode username
        registerUser registration gen email (Just verificationCode) >>= returnUserById
  -- User should provide at least code or email
  -- This CANNOT happend because the parser prevents this!
  -- TODO make this condition impossible via the type system
  | otherwise = error "Either one of [ 'code', 'email' ] has to be provided"

-- TODO check that user is not there
registerUser :: JsonRegistration.Registration
             -> StdGen
             -> Email
             -> Maybe Text
             -> ApiAction ctx (Maybe (Key SqlT.User))
registerUser registration gen mail verificationCode = runSQL $ insertUnique user
    where user = SqlT.User
                  { SqlT.userEmail      = mail
                  , SqlT.userPassword   = hashedSaltedPassword
                  , SqlT.userSalt       = salt
                  , SqlT.userFirstName  = JsonRegistration.firstName registration
                  , SqlT.userLastName   = JsonRegistration.lastName registration
                  , SqlT.userVerifyCode = verificationCode
                  , SqlT.userDisabled   = False
                  , SqlT.userAbsent     = JsonRegistration.absent registration
                  }
          pw = JsonRegistration.password registration
          salt = Crypto.passwordSalt gen
          hashedSaltedPassword = Crypto.hashPassword pw salt

currentUserAction :: ListContains n Email xs => ApiAction (HVect xs) a
currentUserAction = getCurrentUser >>= json . JsonUser.jsonUser
