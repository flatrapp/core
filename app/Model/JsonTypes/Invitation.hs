{-# LANGUAGE DeriveGeneric #-}

module Model.JsonTypes.Invitation
    ( Invitation(..)
    , jsonInvitation
    )
where

import           Data.Aeson           (ToJSON)
import           Data.Text            (Text)
import           Database.Persist.Sql (Entity(..))
import           GHC.Generics         (Generic)
import           Prelude              hiding (id)

import qualified Model.SqlTypes       as SqlT
import           Query.Util           (integerKey)

data Invitation =
     Invitation { id             :: Integer
                , email          :: Text
                , invitationCode :: Text
                } deriving (Show, Generic)

instance ToJSON Invitation

jsonInvitation :: Entity SqlT.Invitation -> Invitation
jsonInvitation (Entity invitationId invitation) =
    Invitation { id             = integerKey invitationId
               , email          = SqlT.invitationEmail invitation
               , invitationCode = SqlT.invitationCode invitation
               }
