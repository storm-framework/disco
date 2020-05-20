{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

{-@ LIQUID "--compile-spec" @-}

module Model 
  ( EntityFieldWrapper(..)
  , migrateAll
  , BinahRecord
  , persistentRecord
  , mkInvitation
  , mkUser
  , Invitation
  , User
  , invitationId'
  , invitationCode'
  , invitationEmailAddress'
  , invitationActivated'
  , userId'
  , userUsername'
  , userPassword'
  , userFullName'
  , userAffiliation'
  , userEmailAddress'
  , userLevel'
  , InvitationId
  , UserId
  )

where

import           Database.Persist               ( Key )
import           Database.Persist.TH            ( share
                                                , mkMigrate
                                                , mkPersist
                                                , sqlSettings
                                                , persistLowerCase
                                                )
import           Data.Text                      ( Text )
import qualified Database.Persist              as Persist

import           Binah.Core

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Invitation
  code Text
  emailAddress Text
  activated Bool

User
  username Text
  password Text
  fullName Text
  affiliation Text
  emailAddress Text
  level String
|]

{-@
data EntityFieldWrapper record typ < querypolicy :: Entity record -> Entity User -> Bool
                                   , selector :: Entity record -> typ -> Bool
                                   , flippedselector :: typ -> Entity record -> Bool
                                   , capability :: Entity record -> Bool
                                   , updatepolicy :: Entity record -> Entity record -> Entity User -> Bool
                                   > = EntityFieldWrapper _
@-}

data EntityFieldWrapper record typ = EntityFieldWrapper (Persist.EntityField record typ)
{-@ data variance EntityFieldWrapper covariant covariant invariant invariant invariant invariant invariant @-}

{-@ measure currentUser :: Entity User @-}

--------------------------------------------------------------------------------
-- | Predicates
--------------------------------------------------------------------------------



--------------------------------------------------------------------------------
-- | Policies
--------------------------------------------------------------------------------

{-@ predicate IsSelf USER VIEWER = USER == VIEWER @-}

{-@ predicate IsOrganizer USER = userLevel (entityVal USER) == "organizer" @-}

--------------------------------------------------------------------------------
-- | Records
--------------------------------------------------------------------------------

{-@ data BinahRecord record < 
    p :: Entity record -> Bool
  , insertpolicy :: Entity record -> Entity User -> Bool
  , querypolicy  :: Entity record -> Entity User -> Bool 
  >
  = BinahRecord _
@-}
data BinahRecord record = BinahRecord record
{-@ data variance BinahRecord invariant invariant invariant invariant @-}

{-@ persistentRecord :: BinahRecord record -> record @-}
persistentRecord :: BinahRecord record -> record
persistentRecord (BinahRecord record) = record

{-@ measure getJust :: Key record -> Entity record @-}

-- * Invitation
{-@ mkInvitation :: 
     x_0: Text
  -> x_1: Text
  -> x_2: Bool
  -> BinahRecord < 
       {\row -> invitationCode (entityVal row) == x_0 && invitationEmailAddress (entityVal row) == x_1 && invitationActivated (entityVal row) == x_2}
     , {\invitation viewer -> not (invitationActivated (entityVal invitation)) && IsOrganizer viewer}
     , {\x_0 x_1 -> False}
     > Invitation
@-}
mkInvitation x_0 x_1 x_2 = BinahRecord (Invitation x_0 x_1 x_2)

{-@ invariant {v: Entity Invitation | v == getJust (entityKey v)} @-}



{-@ assume invitationId' :: EntityFieldWrapper <
    {\row viewer -> True}
  , {\row field  -> field == entityKey row}
  , {\field row  -> field == entityKey row}
  , {\_ -> False}
  , {\_ _ _ -> True}
  > _ _
@-}
invitationId' :: EntityFieldWrapper Invitation InvitationId
invitationId' = EntityFieldWrapper InvitationId

{-@ measure invitationCode :: Invitation -> Text @-}

{-@ measure invitationCodeCap :: Entity Invitation -> Bool @-}

{-@ assume invitationCode' :: EntityFieldWrapper <
    {\_ _ -> True}
  , {\row field  -> field == invitationCode (entityVal row)}
  , {\field row  -> field == invitationCode (entityVal row)}
  , {\old -> invitationCodeCap old}
  , {\old _ _ -> invitationCodeCap old}
  > _ _
@-}
invitationCode' :: EntityFieldWrapper Invitation Text
invitationCode' = EntityFieldWrapper InvitationCode

{-@ measure invitationEmailAddress :: Invitation -> Text @-}

{-@ measure invitationEmailAddressCap :: Entity Invitation -> Bool @-}

{-@ assume invitationEmailAddress' :: EntityFieldWrapper <
    {\_ _ -> True}
  , {\row field  -> field == invitationEmailAddress (entityVal row)}
  , {\field row  -> field == invitationEmailAddress (entityVal row)}
  , {\old -> invitationEmailAddressCap old}
  , {\old _ _ -> invitationEmailAddressCap old}
  > _ _
@-}
invitationEmailAddress' :: EntityFieldWrapper Invitation Text
invitationEmailAddress' = EntityFieldWrapper InvitationEmailAddress

{-@ measure invitationActivated :: Invitation -> Bool @-}

{-@ measure invitationActivatedCap :: Entity Invitation -> Bool @-}

{-@ assume invitationActivated' :: EntityFieldWrapper <
    {\_ _ -> True}
  , {\row field  -> field == invitationActivated (entityVal row)}
  , {\field row  -> field == invitationActivated (entityVal row)}
  , {\old -> invitationActivatedCap old}
  , {\old _ _ -> invitationActivatedCap old}
  > _ _
@-}
invitationActivated' :: EntityFieldWrapper Invitation Bool
invitationActivated' = EntityFieldWrapper InvitationActivated

-- * User
{-@ mkUser :: 
     x_0: Text
  -> x_1: Text
  -> x_2: Text
  -> x_3: Text
  -> x_4: Text
  -> x_5: String
  -> BinahRecord < 
       {\row -> userUsername (entityVal row) == x_0 && userPassword (entityVal row) == x_1 && userFullName (entityVal row) == x_2 && userAffiliation (entityVal row) == x_3 && userEmailAddress (entityVal row) == x_4 && userLevel (entityVal row) == x_5}
     , {\_ _ -> True}
     , {\x_0 x_1 -> (x_0 == x_1)}
     > User
@-}
mkUser x_0 x_1 x_2 x_3 x_4 x_5 = BinahRecord (User x_0 x_1 x_2 x_3 x_4 x_5)

{-@ invariant {v: Entity User | v == getJust (entityKey v)} @-}



{-@ assume userId' :: EntityFieldWrapper <
    {\row viewer -> True}
  , {\row field  -> field == entityKey row}
  , {\field row  -> field == entityKey row}
  , {\_ -> False}
  , {\_ _ _ -> True}
  > _ _
@-}
userId' :: EntityFieldWrapper User UserId
userId' = EntityFieldWrapper UserId

{-@ measure userUsername :: User -> Text @-}

{-@ measure userUsernameCap :: Entity User -> Bool @-}

{-@ assume userUsername' :: EntityFieldWrapper <
    {\_ _ -> True}
  , {\row field  -> field == userUsername (entityVal row)}
  , {\field row  -> field == userUsername (entityVal row)}
  , {\old -> userUsernameCap old}
  , {\old _ _ -> userUsernameCap old}
  > _ _
@-}
userUsername' :: EntityFieldWrapper User Text
userUsername' = EntityFieldWrapper UserUsername

{-@ measure userPassword :: User -> Text @-}

{-@ measure userPasswordCap :: Entity User -> Bool @-}

{-@ assume userPassword' :: EntityFieldWrapper <
    {\x_0 x_1 -> (x_0 == x_1)}
  , {\row field  -> field == userPassword (entityVal row)}
  , {\field row  -> field == userPassword (entityVal row)}
  , {\old -> userPasswordCap old}
  , {\old _ _ -> userPasswordCap old}
  > _ _
@-}
userPassword' :: EntityFieldWrapper User Text
userPassword' = EntityFieldWrapper UserPassword

{-@ measure userFullName :: User -> Text @-}

{-@ measure userFullNameCap :: Entity User -> Bool @-}

{-@ assume userFullName' :: EntityFieldWrapper <
    {\_ _ -> True}
  , {\row field  -> field == userFullName (entityVal row)}
  , {\field row  -> field == userFullName (entityVal row)}
  , {\old -> userFullNameCap old}
  , {\old _ _ -> userFullNameCap old}
  > _ _
@-}
userFullName' :: EntityFieldWrapper User Text
userFullName' = EntityFieldWrapper UserFullName

{-@ measure userAffiliation :: User -> Text @-}

{-@ measure userAffiliationCap :: Entity User -> Bool @-}

{-@ assume userAffiliation' :: EntityFieldWrapper <
    {\_ _ -> True}
  , {\row field  -> field == userAffiliation (entityVal row)}
  , {\field row  -> field == userAffiliation (entityVal row)}
  , {\old -> userAffiliationCap old}
  , {\old _ _ -> userAffiliationCap old}
  > _ _
@-}
userAffiliation' :: EntityFieldWrapper User Text
userAffiliation' = EntityFieldWrapper UserAffiliation

{-@ measure userEmailAddress :: User -> Text @-}

{-@ measure userEmailAddressCap :: Entity User -> Bool @-}

{-@ assume userEmailAddress' :: EntityFieldWrapper <
    {\_ _ -> True}
  , {\row field  -> field == userEmailAddress (entityVal row)}
  , {\field row  -> field == userEmailAddress (entityVal row)}
  , {\old -> userEmailAddressCap old}
  , {\old _ _ -> userEmailAddressCap old}
  > _ _
@-}
userEmailAddress' :: EntityFieldWrapper User Text
userEmailAddress' = EntityFieldWrapper UserEmailAddress

{-@ measure userLevel :: User -> String @-}

{-@ measure userLevelCap :: Entity User -> Bool @-}

{-@ assume userLevel' :: EntityFieldWrapper <
    {\_ _ -> True}
  , {\row field  -> field == userLevel (entityVal row)}
  , {\field row  -> field == userLevel (entityVal row)}
  , {\old -> userLevelCap old}
  , {\old _ _ -> userLevelCap old}
  > _ _
@-}
userLevel' :: EntityFieldWrapper User String
userLevel' = EntityFieldWrapper UserLevel

--------------------------------------------------------------------------------
-- | Inline
--------------------------------------------------------------------------------


