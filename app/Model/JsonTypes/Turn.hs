{-# LANGUAGE DeriveGeneric #-}

module Model.JsonTypes.Turn where

import           Data.Aeson           hiding (json)
import           Database.Persist.Sql
import           Data.Time.Clock      (UTCTime)
import           GHC.Generics
import qualified Model.CoreTypes      as SqlT
import           Prelude              hiding (id)
import qualified Util

data Turn =
    Turn { userId :: Integer  -- UserId
         , startDate   :: UTCTime
         } deriving (Show, Generic)

instance ToJSON Turn
instance FromJSON Turn

jsonTurn :: Entity SqlT.Turn -> Turn
jsonTurn (Entity _turnId turn) =
    Turn { userId    = Util.integerKey . SqlT.turnUserId $ turn
         , startDate = SqlT.turnStartDate turn
         }
