module Query.Invitation
    ( getInvitations
    )
where

import qualified Database.Persist as P

import           Model.SqlTypes   --(InvitationId, Invitation(..))
import           Model.CoreTypes  (SqlQuery)

getInvitations :: SqlQuery [P.Entity Invitation]
getInvitations = P.selectList [] [P.Asc InvitationId]
