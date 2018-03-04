{-# LANGUAGE DeriveGeneric #-}

module Model.JsonTypes.Turn where

import           Data.Aeson           (ToJSON)
import           Database.Persist.Sql (Entity(..))
import           Data.Time.Clock      (UTCTime)
import           GHC.Generics         (Generic)
import qualified Model.SqlTypes       as SqlT
import qualified Util

data Turn =
    Turn { userId    :: Integer
         , startDate :: UTCTime
         } deriving (Show, Generic)

instance ToJSON Turn

jsonTurn :: Entity SqlT.Turn -> Turn
jsonTurn (Entity _turnId turn) =
    Turn { userId    = Util.integerKey . SqlT.turnUserId $ turn
         , startDate = SqlT.turnStartDate turn
         }
