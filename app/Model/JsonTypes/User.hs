{-# LANGUAGE DeriveGeneric #-}

module Model.JsonTypes.User
    ( User(..)
    , jsonUser
    )
where

import           Data.Aeson           (ToJSON)
import           Data.Text            (Text)
import           Database.Persist.Sql (Entity(..))
import           GHC.Generics         (Generic)
import qualified Model.SqlTypes       as SqlT
import           Prelude              hiding (id)
import qualified Util

data User =
    User { id            :: Integer
         , email         :: Text
         , firstName     :: Text
         , lastName      :: Text
         , emailVerified :: Bool
         , disabled      :: Bool
         , absent        :: Bool
         } deriving (Show, Generic)

instance ToJSON User

jsonUser :: Entity SqlT.User -> User
jsonUser (Entity userId user) =
    User { id            = Util.integerKey userId
         , email         = SqlT.userEmail user
         , firstName     = SqlT.userFirstName user
         , lastName      = SqlT.userLastName user
         , emailVerified = SqlT.userIsVerified user
         , disabled      = SqlT.userDisabled user
         , absent        = SqlT.userAbsent user
         }
