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
  , mkRoom
  , Invitation
  , User
  , Room
  , invitationId'
  , invitationCode'
  , invitationEmailAddress'
  , invitationFirstName'
  , invitationLastName'
  , invitationInstitution'
  , invitationAccepted'
  , invitationEmailStatus'
  , invitationEmailError'
  , userId'
  , userEmailAddress'
  , userPassword'
  , userPhotoURL'
  , userFirstName'
  , userLastName'
  , userDisplayName'
  , userInstitution'
  , userLevel'
  , userVisibility'
  , userRoom'
  , roomId'
  , roomColor'
  , roomName'
  , roomCapacity'
  , roomZoomLink'
  , InvitationId
  , UserId
  , RoomId
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
  firstName Text
  lastName Text
  institution Text
  accepted Bool
  emailStatus String
  emailError String Maybe

User
  emailAddress Text
  password Text
  photoURL Text Maybe
  firstName Text
  lastName Text
  displayName Text
  institution Text
  level String
  visibility String
  room RoomId Maybe

Room
  color Text
  name Text
  capacity Int
  zoomLink Text
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
{-@ data variance BinahRecord invariant covariant invariant invariant @-}

{-@ persistentRecord :: BinahRecord record -> record @-}
persistentRecord :: BinahRecord record -> record
persistentRecord (BinahRecord record) = record

{-@ measure getJust :: Key record -> Entity record @-}

-- * Invitation
{-@ mkInvitation ::
     x_0: Text
  -> x_1: Text
  -> x_2: Text
  -> x_3: Text
  -> x_4: Text
  -> x_5: Bool
  -> x_6: String
  -> x_7: (Maybe String)
  -> BinahRecord <
       {\row -> invitationCode (entityVal row) == x_0 && invitationEmailAddress (entityVal row) == x_1 && invitationFirstName (entityVal row) == x_2 && invitationLastName (entityVal row) == x_3 && invitationInstitution (entityVal row) == x_4 && invitationAccepted (entityVal row) == x_5 && invitationEmailStatus (entityVal row) == x_6 && invitationEmailError (entityVal row) == x_7}
     , {\invitation viewer -> not (invitationAccepted (entityVal invitation)) && IsOrganizer viewer && emailStatus invitation == "not_sent"}
     , {\x_0 x_1 -> False}
     > Invitation
@-}
mkInvitation x_0 x_1 x_2 x_3 x_4 x_5 x_6 x_7 = BinahRecord (Invitation x_0 x_1 x_2 x_3 x_4 x_5 x_6 x_7)

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

{-@ measure invitationFirstName :: Invitation -> Text @-}

{-@ measure invitationFirstNameCap :: Entity Invitation -> Bool @-}

{-@ assume invitationFirstName' :: EntityFieldWrapper <
    {\_ _ -> True}
  , {\row field  -> field == invitationFirstName (entityVal row)}
  , {\field row  -> field == invitationFirstName (entityVal row)}
  , {\old -> invitationFirstNameCap old}
  , {\old _ _ -> invitationFirstNameCap old}
  > _ _
@-}
invitationFirstName' :: EntityFieldWrapper Invitation Text
invitationFirstName' = EntityFieldWrapper InvitationFirstName

{-@ measure invitationLastName :: Invitation -> Text @-}

{-@ measure invitationLastNameCap :: Entity Invitation -> Bool @-}

{-@ assume invitationLastName' :: EntityFieldWrapper <
    {\_ _ -> True}
  , {\row field  -> field == invitationLastName (entityVal row)}
  , {\field row  -> field == invitationLastName (entityVal row)}
  , {\old -> invitationLastNameCap old}
  , {\old _ _ -> invitationLastNameCap old}
  > _ _
@-}
invitationLastName' :: EntityFieldWrapper Invitation Text
invitationLastName' = EntityFieldWrapper InvitationLastName

{-@ measure invitationInstitution :: Invitation -> Text @-}

{-@ measure invitationInstitutionCap :: Entity Invitation -> Bool @-}

{-@ assume invitationInstitution' :: EntityFieldWrapper <
    {\_ _ -> True}
  , {\row field  -> field == invitationInstitution (entityVal row)}
  , {\field row  -> field == invitationInstitution (entityVal row)}
  , {\old -> invitationInstitutionCap old}
  , {\old _ _ -> invitationInstitutionCap old}
  > _ _
@-}
invitationInstitution' :: EntityFieldWrapper Invitation Text
invitationInstitution' = EntityFieldWrapper InvitationInstitution

{-@ measure invitationAccepted :: Invitation -> Bool @-}

{-@ measure invitationAcceptedCap :: Entity Invitation -> Bool @-}

{-@ assume invitationAccepted' :: EntityFieldWrapper <
    {\_ _ -> True}
  , {\row field  -> field == invitationAccepted (entityVal row)}
  , {\field row  -> field == invitationAccepted (entityVal row)}
  , {\old -> invitationAcceptedCap old}
  , {\x_0 x_1 x_2 -> ((not (invitationAccepted (entityVal x_0)) && invitationAccepted (entityVal x_1))) => (invitationAcceptedCap x_0)}
  > _ _
@-}
invitationAccepted' :: EntityFieldWrapper Invitation Bool
invitationAccepted' = EntityFieldWrapper InvitationAccepted

{-@ measure invitationEmailStatus :: Invitation -> String @-}

{-@ measure invitationEmailStatusCap :: Entity Invitation -> Bool @-}

{-@ assume invitationEmailStatus' :: EntityFieldWrapper <
    {\_ _ -> True}
  , {\row field  -> field == invitationEmailStatus (entityVal row)}
  , {\field row  -> field == invitationEmailStatus (entityVal row)}
  , {\old -> invitationEmailStatusCap old}
  , {\x_0 x_1 x_2 -> ((IsOrganizer viewer && (emailStatus x_1 == "sent" || emailStatus x_1 == "error"))) => (invitationEmailStatusCap x_0)}
  > _ _
@-}
invitationEmailStatus' :: EntityFieldWrapper Invitation String
invitationEmailStatus' = EntityFieldWrapper InvitationEmailStatus

{-@ measure invitationEmailError :: Invitation -> (Maybe String) @-}

{-@ measure invitationEmailErrorCap :: Entity Invitation -> Bool @-}

{-@ assume invitationEmailError' :: EntityFieldWrapper <
    {\_ _ -> True}
  , {\row field  -> field == invitationEmailError (entityVal row)}
  , {\field row  -> field == invitationEmailError (entityVal row)}
  , {\old -> invitationEmailErrorCap old}
  , {\old _ _ -> invitationEmailErrorCap old}
  > _ _
@-}
invitationEmailError' :: EntityFieldWrapper Invitation (Maybe String)
invitationEmailError' = EntityFieldWrapper InvitationEmailError

-- * User
{-@ mkUser ::
     x_0: Text
  -> x_1: Text
  -> x_2: (Maybe Text)
  -> x_3: Text
  -> x_4: Text
  -> x_5: Text
  -> x_6: Text
  -> x_7: String
  -> x_8: String
  -> x_9: (Maybe RoomId)
  -> BinahRecord <
       {\row -> userEmailAddress (entityVal row) == x_0 && userPassword (entityVal row) == x_1 && userPhotoURL (entityVal row) == x_2 && userFirstName (entityVal row) == x_3 && userLastName (entityVal row) == x_4 && userDisplayName (entityVal row) == x_5 && userInstitution (entityVal row) == x_6 && userLevel (entityVal row) == x_7 && userVisibility (entityVal row) == x_8 && userRoom (entityVal row) == x_9}
     , {\new viewer -> IsOrganizer viewer || userLevel (entityVal new) == "attendee"}
     , {\x_0 x_1 -> (userVisibility (entityVal x_0) == "public" || IsSelf x_0 x_1) || (x_0 == x_1)}
     > User
@-}
mkUser x_0 x_1 x_2 x_3 x_4 x_5 x_6 x_7 x_8 x_9 = BinahRecord (User x_0 x_1 x_2 x_3 x_4 x_5 x_6 x_7 x_8 x_9)

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

{-@ measure userPhotoURL :: User -> (Maybe Text) @-}

{-@ measure userPhotoURLCap :: Entity User -> Bool @-}

{-@ assume userPhotoURL' :: EntityFieldWrapper <
    {\_ _ -> True}
  , {\row field  -> field == userPhotoURL (entityVal row)}
  , {\field row  -> field == userPhotoURL (entityVal row)}
  , {\old -> userPhotoURLCap old}
  , {\old _ _ -> userPhotoURLCap old}
  > _ _
@-}
userPhotoURL' :: EntityFieldWrapper User (Maybe Text)
userPhotoURL' = EntityFieldWrapper UserPhotoURL

{-@ measure userFirstName :: User -> Text @-}

{-@ measure userFirstNameCap :: Entity User -> Bool @-}

{-@ assume userFirstName' :: EntityFieldWrapper <
    {\_ _ -> True}
  , {\row field  -> field == userFirstName (entityVal row)}
  , {\field row  -> field == userFirstName (entityVal row)}
  , {\old -> userFirstNameCap old}
  , {\old _ _ -> userFirstNameCap old}
  > _ _
@-}
userFirstName' :: EntityFieldWrapper User Text
userFirstName' = EntityFieldWrapper UserFirstName

{-@ measure userLastName :: User -> Text @-}

{-@ measure userLastNameCap :: Entity User -> Bool @-}

{-@ assume userLastName' :: EntityFieldWrapper <
    {\_ _ -> True}
  , {\row field  -> field == userLastName (entityVal row)}
  , {\field row  -> field == userLastName (entityVal row)}
  , {\old -> userLastNameCap old}
  , {\old _ _ -> userLastNameCap old}
  > _ _
@-}
userLastName' :: EntityFieldWrapper User Text
userLastName' = EntityFieldWrapper UserLastName

{-@ measure userDisplayName :: User -> Text @-}

{-@ measure userDisplayNameCap :: Entity User -> Bool @-}

{-@ assume userDisplayName' :: EntityFieldWrapper <
    {\_ _ -> True}
  , {\row field  -> field == userDisplayName (entityVal row)}
  , {\field row  -> field == userDisplayName (entityVal row)}
  , {\old -> userDisplayNameCap old}
  , {\old _ _ -> userDisplayNameCap old}
  > _ _
@-}
userDisplayName' :: EntityFieldWrapper User Text
userDisplayName' = EntityFieldWrapper UserDisplayName

{-@ measure userInstitution :: User -> Text @-}

{-@ measure userInstitutionCap :: Entity User -> Bool @-}

{-@ assume userInstitution' :: EntityFieldWrapper <
    {\_ _ -> True}
  , {\row field  -> field == userInstitution (entityVal row)}
  , {\field row  -> field == userInstitution (entityVal row)}
  , {\old -> userInstitutionCap old}
  , {\old _ _ -> userInstitutionCap old}
  > _ _
@-}
userInstitution' :: EntityFieldWrapper User Text
userInstitution' = EntityFieldWrapper UserInstitution

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

{-@ measure userVisibility :: User -> String @-}

{-@ measure userVisibilityCap :: Entity User -> Bool @-}

{-@ assume userVisibility' :: EntityFieldWrapper <
    {\_ _ -> True}
  , {\row field  -> field == userVisibility (entityVal row)}
  , {\field row  -> field == userVisibility (entityVal row)}
  , {\old -> userVisibilityCap old}
  , {\old _ _ -> userVisibilityCap old}
  > _ _
@-}
userVisibility' :: EntityFieldWrapper User String
userVisibility' = EntityFieldWrapper UserVisibility

{-@ measure userRoom :: User -> (Maybe RoomId) @-}

{-@ measure userRoomCap :: Entity User -> Bool @-}

{-@ assume userRoom' :: EntityFieldWrapper <
    {\x_0 x_1 -> (userVisibility (entityVal x_0) == "public" || IsSelf x_0 x_1)}
  , {\row field  -> field == userRoom (entityVal row)}
  , {\field row  -> field == userRoom (entityVal row)}
  , {\old -> userRoomCap old}
  , {\x_0 x_1 x_2 -> ((IsSelf x_0 x_2)) => (userRoomCap x_0)}
  > _ _
@-}
userRoom' :: EntityFieldWrapper User (Maybe RoomId)
userRoom' = EntityFieldWrapper UserRoom

-- * Room
{-@ mkRoom ::
     x_0: Text
  -> x_1: Text
  -> x_2: Int
  -> x_3: Text
  -> BinahRecord <
       {\row -> roomColor (entityVal row) == x_0 && roomName (entityVal row) == x_1 && roomCapacity (entityVal row) == x_2 && roomZoomLink (entityVal row) == x_3}
     , {\_ viewer -> IsOrganizer viewer}
     , {\x_0 x_1 -> False}
     > Room
@-}
mkRoom x_0 x_1 x_2 x_3 = BinahRecord (Room x_0 x_1 x_2 x_3)

{-@ invariant {v: Entity Room | v == getJust (entityKey v)} @-}



{-@ assume roomId' :: EntityFieldWrapper <
    {\row viewer -> True}
  , {\row field  -> field == entityKey row}
  , {\field row  -> field == entityKey row}
  , {\_ -> False}
  , {\_ _ _ -> True}
  > _ _
@-}
roomId' :: EntityFieldWrapper Room RoomId
roomId' = EntityFieldWrapper RoomId

{-@ measure roomColor :: Room -> Text @-}

{-@ measure roomColorCap :: Entity Room -> Bool @-}

{-@ assume roomColor' :: EntityFieldWrapper <
    {\_ _ -> True}
  , {\row field  -> field == roomColor (entityVal row)}
  , {\field row  -> field == roomColor (entityVal row)}
  , {\old -> roomColorCap old}
  , {\old _ _ -> roomColorCap old}
  > _ _
@-}
roomColor' :: EntityFieldWrapper Room Text
roomColor' = EntityFieldWrapper RoomColor

{-@ measure roomName :: Room -> Text @-}

{-@ measure roomNameCap :: Entity Room -> Bool @-}

{-@ assume roomName' :: EntityFieldWrapper <
    {\_ _ -> True}
  , {\row field  -> field == roomName (entityVal row)}
  , {\field row  -> field == roomName (entityVal row)}
  , {\old -> roomNameCap old}
  , {\x_0 x_1 x_2 -> ((IsOrganizer x_2)) => (roomNameCap x_0)}
  > _ _
@-}
roomName' :: EntityFieldWrapper Room Text
roomName' = EntityFieldWrapper RoomName

{-@ measure roomCapacity :: Room -> Int @-}

{-@ measure roomCapacityCap :: Entity Room -> Bool @-}

{-@ assume roomCapacity' :: EntityFieldWrapper <
    {\_ _ -> True}
  , {\row field  -> field == roomCapacity (entityVal row)}
  , {\field row  -> field == roomCapacity (entityVal row)}
  , {\old -> roomCapacityCap old}
  , {\x_0 x_1 x_2 -> ((IsOrganizer x_2)) => (roomCapacityCap x_0)}
  > _ _
@-}
roomCapacity' :: EntityFieldWrapper Room Int
roomCapacity' = EntityFieldWrapper RoomCapacity

{-@ measure roomZoomLink :: Room -> Text @-}

{-@ measure roomZoomLinkCap :: Entity Room -> Bool @-}

{-@ assume roomZoomLink' :: EntityFieldWrapper <
    {\_ _ -> True}
  , {\row field  -> field == roomZoomLink (entityVal row)}
  , {\field row  -> field == roomZoomLink (entityVal row)}
  , {\old -> roomZoomLinkCap old}
  , {\x_0 x_1 x_2 -> ((IsOrganizer x_2)) => (roomZoomLinkCap x_0)}
  > _ _
@-}
roomZoomLink' :: EntityFieldWrapper Room Text
roomZoomLink' = EntityFieldWrapper RoomZoomLink

--------------------------------------------------------------------------------
-- | Inline
--------------------------------------------------------------------------------


