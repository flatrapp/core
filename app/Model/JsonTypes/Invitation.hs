{-# LANGUAGE DeriveGeneric #-}

module Model.JsonTypes.Invitation where

import           Data.Aeson           hiding (json)
import           Data.Text            (Text)
import           Database.Persist.Sql
import           GHC.Generics
import qualified Model.SqlTypes       as SqlT
import           Prelude              hiding (id)
import           Util

data Invitation =
     Invitation { id             :: Maybe Integer
                , email          :: Text
                , invitationCode :: Maybe Text
                } deriving (Show, Generic)

instance ToJSON Invitation
instance FromJSON Invitation

jsonInvitation :: Entity SqlT.Invitation -> Invitation
jsonInvitation (Entity invitationId invitation) =
    Invitation { id             = Just $ Util.integerKey invitationId
               , email          = SqlT.invitationEmail invitation
               , invitationCode = SqlT.invitationCode invitation
               }
