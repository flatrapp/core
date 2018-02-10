{-# LANGUAGE DeriveGeneric #-}

module Model.JsonTypes.User where

import           Data.Aeson           hiding (json)
import           Data.Text            (Text)
import           Database.Persist.Sql
import           GHC.Generics
import qualified Model.CoreTypes      as SqlT
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
         , emailVerified = SqlT.userVerified user
         , disabled      = SqlT.userDisabled user
         , absent        = SqlT.userAbsent user
         }
