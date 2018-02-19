{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Web.Endpoints.Users where

import qualified Config                       as Cfg
import           Control.Monad.IO.Class
import           Data.HVect                   (HVect, ListContains, findFirst)
import           Data.Text                    (Text)
import           Database.Persist             hiding (delete, get)
import qualified Database.Persist             as P
import           Model.CoreTypes              (ApiAction, Api, Email)
import qualified Model.SqlTypes               as SqlT
import qualified Model.JsonTypes.Registration as JsonRegistration
import qualified Model.JsonTypes.User         as JsonUser
import           Network.HTTP.Types.Status    (created201)
import           System.Random
import           Util                         (errorJson, runSQL)
import qualified Util
import           Web.Spock

-- TODO restrict all endpoints to logged in users EXCEPT post "users"
routeUsers :: Cfg.FlatrCfg -> Api ctx
routeUsers cfg = do  -- TODO use cfg from State Monad somehow
  get "users" getUsersAction
  get ("users" <//> var) $ returnUserById . Just
  delete ("user" <//> var) $ \userId ->
    Util.trySqlGet userId >> deleteUserAction userId
  -- TOOD implement put "users" $ do
  post "users" $
    Util.eitherJsonBody >>= postUsersAction cfg  -- TODO use the Nothing case as intermediate action and chain it in somehow

returnUserById :: Maybe (Key SqlT.User) -> ApiAction ctx m
returnUserById Nothing =
  -- TODO combine with registration... because this function is also called once when there is no registration happening
  Util.errorJson Util.UserEmailExists
returnUserById (Just userId) =
  Util.trySqlSelectFirst SqlT.UserId userId >>= json . JsonUser.jsonUser

getUsersAction :: ApiAction ctx a
getUsersAction =
  json =<< (map JsonUser.jsonUser <$> runSQL (selectList [] [Asc SqlT.UserId]))

deleteUserAction :: SqlT.UserId -> ApiAction ctx a
deleteUserAction userId = do
  runSQL $ P.delete userId
  Util.emptyResponse

postUsersAction :: Cfg.FlatrCfg -> JsonRegistration.Registration -> ApiAction ctx a
postUsersAction cfg registration
  | Just code <- JsonRegistration.invitationCode registration = do
      -- fails if user provided code but code is not in DB
      (Entity invitationId theInvitation) <- Util.trySqlSelectFirstError Util.InvitationCodeInvalid SqlT.InvitationCode $ Just code
      let email = SqlT.invitationEmail theInvitation
      -- fails if user exists
      _user <- Util.trySqlSelectFirstError Util.UserEmailExists SqlT.UserEmail email
      -- remove Invitation after it has been used
      runSQL $ P.delete invitationId
      setStatus created201
      gen <- liftIO getStdGen
      newId <- registerUser registration gen email True
      returnUserById newId

  | Just email <- JsonRegistration.email registration = do  -- no invitationCode provided
      gen <- liftIO getStdGen
      if email `elem` Cfg.whitelistedMails cfg then do
        setStatus created201
        newId <- registerUser registration gen email True
        returnUserById newId
      else do
        -- fails if user provided email but is not invited
        _inv <- Util.trySqlSelectFirstError Util.NotInvited SqlT.InvitationEmail email
        -- fails if user exists
        _user <- Util.trySqlSelectFirstError Util.UserEmailExists SqlT.UserEmail email
        -- TODO send verification Email if smtp config set
        -- $frontedUrl/#signup?code=$code&serverUrl=$serverUrl
        registerUser registration gen email False >>= returnUserById

  | otherwise = -- User should provide at least code or email
      Util.errorJson $ Util.BadRequest "Either one of [ 'code', 'email' ] has to be provided"

-- TODO check that user is not there
registerUser :: JsonRegistration.Registration
             -> StdGen
             -> Email
             -> Bool
             -> ApiAction ctx (Maybe (Key SqlT.User))
registerUser registration gen mail verified = runSQL $ insertUnique user
    where user = SqlT.User
                    { SqlT.userEmail     = mail
                    , SqlT.userPassword  = hashedSaltedPassword
                    , SqlT.userSalt      = Util.makeHex salt'
                    , SqlT.userFirstName = JsonRegistration.firstName registration
                    , SqlT.userLastName  = JsonRegistration.lastName registration
                    , SqlT.userVerified  = verified
                    , SqlT.userDisabled  = False
                    , SqlT.userAbsent    = JsonRegistration.absent registration
                    }
          pw = JsonRegistration.password registration
          salt' = Util.randomBS 512 gen
          hashedSaltedPassword = Util.hashPassword pw salt'


currentUserAction :: ListContains n Email xs => ApiAction (HVect xs) a
currentUserAction = do
  (email :: Text) <- fmap findFirst getContext
  Util.trySqlSelectFirst' SqlT.UserEmail email >>= json . JsonUser.jsonUser
