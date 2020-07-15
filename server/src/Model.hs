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
  , mkMessage
  , mkMarkRead
  , Invitation
  , User
  , Room
  , Message
  , MarkRead
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
  , userDisplayName'
  , userInstitution'
  , userPronouns'
  , userWebsite'
  , userBio'
  , userLevel'
  , userVisibility'
  , userRoom'
  , roomId'
  , roomColor'
  , roomName'
  , roomTopic'
  , roomZoomLink'
  , messageId'
  , messageSender'
  , messageReceiver'
  , messageMessage'
  , messageTimestamp'
  , markReadId'
  , markReadUser'
  , markReadUpto'
  , InvitationId
  , UserId
  , RoomId
  , MessageId
  , MarkReadId
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

import Data.ByteString (ByteString)
import Data.Int (Int64)

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
  UniqueInvitationEmailAddress emailAddress

User
  emailAddress Text
  password ByteString
  photoURL Text Maybe
  displayName Text
  institution Text
  pronouns Text
  website Text
  bio Text
  level String
  visibility String
  room RoomId Maybe
  UniqueUserEmailAddress emailAddress

Room
  color Text
  name Text
  topic Text
  zoomLink Text
  

Message
  sender UserId
  receiver UserId Maybe
  message Text
  timestamp Int64
  

MarkRead
  user UserId
  upto MessageId
  
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

{-@ predicate IsInRoom VIEWER ROOM = userRoom (entityVal VIEWER) == Just (entityKey ROOM) @-}

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
     , {\invitation viewer -> not (invitationAccepted (entityVal invitation)) && IsOrganizer viewer && invitationEmailStatus (entityVal invitation) == "not_sent"}
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
  > Invitation InvitationId
@-}
invitationId' :: EntityFieldWrapper Invitation InvitationId
invitationId' = EntityFieldWrapper InvitationId

{-@ measure invitationCode :: Invitation -> Text @-}

{-@ measure invitationCodeCap :: Entity Invitation -> Bool @-}

{-@ assume invitationCode' :: EntityFieldWrapper <
    {\_ _ -> True}
  , {\row field -> field == invitationCode (entityVal row)}
  , {\field row -> field == invitationCode (entityVal row)}
  , {\old -> invitationCodeCap old}
  , {\old _ _ -> invitationCodeCap old}
  > Invitation Text
@-}
invitationCode' :: EntityFieldWrapper Invitation Text
invitationCode' = EntityFieldWrapper InvitationCode

{-@ measure invitationEmailAddress :: Invitation -> Text @-}

{-@ measure invitationEmailAddressCap :: Entity Invitation -> Bool @-}

{-@ assume invitationEmailAddress' :: EntityFieldWrapper <
    {\_ _ -> True}
  , {\row field -> field == invitationEmailAddress (entityVal row)}
  , {\field row -> field == invitationEmailAddress (entityVal row)}
  , {\old -> invitationEmailAddressCap old}
  , {\old _ _ -> invitationEmailAddressCap old}
  > Invitation Text
@-}
invitationEmailAddress' :: EntityFieldWrapper Invitation Text
invitationEmailAddress' = EntityFieldWrapper InvitationEmailAddress

{-@ measure invitationFirstName :: Invitation -> Text @-}

{-@ measure invitationFirstNameCap :: Entity Invitation -> Bool @-}

{-@ assume invitationFirstName' :: EntityFieldWrapper <
    {\_ _ -> True}
  , {\row field -> field == invitationFirstName (entityVal row)}
  , {\field row -> field == invitationFirstName (entityVal row)}
  , {\old -> invitationFirstNameCap old}
  , {\old _ _ -> invitationFirstNameCap old}
  > Invitation Text
@-}
invitationFirstName' :: EntityFieldWrapper Invitation Text
invitationFirstName' = EntityFieldWrapper InvitationFirstName

{-@ measure invitationLastName :: Invitation -> Text @-}

{-@ measure invitationLastNameCap :: Entity Invitation -> Bool @-}

{-@ assume invitationLastName' :: EntityFieldWrapper <
    {\_ _ -> True}
  , {\row field -> field == invitationLastName (entityVal row)}
  , {\field row -> field == invitationLastName (entityVal row)}
  , {\old -> invitationLastNameCap old}
  , {\old _ _ -> invitationLastNameCap old}
  > Invitation Text
@-}
invitationLastName' :: EntityFieldWrapper Invitation Text
invitationLastName' = EntityFieldWrapper InvitationLastName

{-@ measure invitationInstitution :: Invitation -> Text @-}

{-@ measure invitationInstitutionCap :: Entity Invitation -> Bool @-}

{-@ assume invitationInstitution' :: EntityFieldWrapper <
    {\_ _ -> True}
  , {\row field -> field == invitationInstitution (entityVal row)}
  , {\field row -> field == invitationInstitution (entityVal row)}
  , {\old -> invitationInstitutionCap old}
  , {\old _ _ -> invitationInstitutionCap old}
  > Invitation Text
@-}
invitationInstitution' :: EntityFieldWrapper Invitation Text
invitationInstitution' = EntityFieldWrapper InvitationInstitution

{-@ measure invitationAccepted :: Invitation -> Bool @-}

{-@ measure invitationAcceptedCap :: Entity Invitation -> Bool @-}

{-@ assume invitationAccepted' :: EntityFieldWrapper <
    {\_ _ -> True}
  , {\row field -> field == invitationAccepted (entityVal row)}
  , {\field row -> field == invitationAccepted (entityVal row)}
  , {\old -> invitationAcceptedCap old}
  , {\x_0 x_1 x_2 -> ((not (invitationAccepted (entityVal x_0)) && invitationAccepted (entityVal x_1))) => (invitationAcceptedCap x_0)}
  > Invitation Bool
@-}
invitationAccepted' :: EntityFieldWrapper Invitation Bool
invitationAccepted' = EntityFieldWrapper InvitationAccepted

{-@ measure invitationEmailStatus :: Invitation -> String @-}

{-@ measure invitationEmailStatusCap :: Entity Invitation -> Bool @-}

{-@ assume invitationEmailStatus' :: EntityFieldWrapper <
    {\_ _ -> True}
  , {\row field -> field == invitationEmailStatus (entityVal row)}
  , {\field row -> field == invitationEmailStatus (entityVal row)}
  , {\old -> invitationEmailStatusCap old}
  , {\x_0 x_1 x_2 -> (((invitationEmailStatus (entityVal x_1) == "sent" || invitationEmailStatus (entityVal x_1) == "error"))) => (invitationEmailStatusCap x_0)}
  > Invitation String
@-}
invitationEmailStatus' :: EntityFieldWrapper Invitation String
invitationEmailStatus' = EntityFieldWrapper InvitationEmailStatus

{-@ measure invitationEmailError :: Invitation -> (Maybe String) @-}

{-@ measure invitationEmailErrorCap :: Entity Invitation -> Bool @-}

{-@ assume invitationEmailError' :: EntityFieldWrapper <
    {\_ _ -> True}
  , {\row field -> field == invitationEmailError (entityVal row)}
  , {\field row -> field == invitationEmailError (entityVal row)}
  , {\old -> invitationEmailErrorCap old}
  , {\old _ _ -> invitationEmailErrorCap old}
  > Invitation (Maybe String)
@-}
invitationEmailError' :: EntityFieldWrapper Invitation (Maybe String)
invitationEmailError' = EntityFieldWrapper InvitationEmailError

-- * User
{-@ mkUser ::
     x_0: Text
  -> x_1: ByteString
  -> x_2: (Maybe Text)
  -> x_3: Text
  -> x_4: Text
  -> x_5: Text
  -> x_6: Text
  -> x_7: Text
  -> x_8: String
  -> x_9: String
  -> x_10: (Maybe RoomId)
  -> BinahRecord <
       {\row -> userEmailAddress (entityVal row) == x_0 && userPassword (entityVal row) == x_1 && userPhotoURL (entityVal row) == x_2 && userDisplayName (entityVal row) == x_3 && userInstitution (entityVal row) == x_4 && userPronouns (entityVal row) == x_5 && userWebsite (entityVal row) == x_6 && userBio (entityVal row) == x_7 && userLevel (entityVal row) == x_8 && userVisibility (entityVal row) == x_9 && userRoom (entityVal row) == x_10}
     , {\new viewer -> IsOrganizer viewer || userLevel (entityVal new) == "attendee"}
     , {\x_0 x_1 -> (userVisibility (entityVal x_0) == "public" || IsSelf x_0 x_1) || (x_0 == x_1)}
     > User
@-}
mkUser x_0 x_1 x_2 x_3 x_4 x_5 x_6 x_7 x_8 x_9 x_10 = BinahRecord (User x_0 x_1 x_2 x_3 x_4 x_5 x_6 x_7 x_8 x_9 x_10)

{-@ invariant {v: Entity User | v == getJust (entityKey v)} @-}



{-@ assume userId' :: EntityFieldWrapper <
    {\row viewer -> True}
  , {\row field  -> field == entityKey row}
  , {\field row  -> field == entityKey row}
  , {\_ -> False}
  , {\_ _ _ -> True}
  > User UserId
@-}
userId' :: EntityFieldWrapper User UserId
userId' = EntityFieldWrapper UserId

{-@ measure userEmailAddress :: User -> Text @-}

{-@ measure userEmailAddressCap :: Entity User -> Bool @-}

{-@ assume userEmailAddress' :: EntityFieldWrapper <
    {\_ _ -> True}
  , {\row field -> field == userEmailAddress (entityVal row)}
  , {\field row -> field == userEmailAddress (entityVal row)}
  , {\old -> userEmailAddressCap old}
  , {\x_0 x_1 x_2 -> ((False)) => (userEmailAddressCap x_0)}
  > User Text
@-}
userEmailAddress' :: EntityFieldWrapper User Text
userEmailAddress' = EntityFieldWrapper UserEmailAddress

{-@ measure userPassword :: User -> ByteString @-}

{-@ measure userPasswordCap :: Entity User -> Bool @-}

{-@ assume userPassword' :: EntityFieldWrapper <
    {\x_0 x_1 -> (x_0 == x_1)}
  , {\row field -> field == userPassword (entityVal row)}
  , {\field row -> field == userPassword (entityVal row)}
  , {\old -> userPasswordCap old}
  , {\x_0 x_1 x_2 -> ((False)) => (userPasswordCap x_0)}
  > User ByteString
@-}
userPassword' :: EntityFieldWrapper User ByteString
userPassword' = EntityFieldWrapper UserPassword

{-@ measure userPhotoURL :: User -> (Maybe Text) @-}

{-@ measure userPhotoURLCap :: Entity User -> Bool @-}

{-@ assume userPhotoURL' :: EntityFieldWrapper <
    {\_ _ -> True}
  , {\row field -> field == userPhotoURL (entityVal row)}
  , {\field row -> field == userPhotoURL (entityVal row)}
  , {\old -> userPhotoURLCap old}
  , {\x_0 x_1 x_2 -> ((IsSelf x_0 x_2)) => (userPhotoURLCap x_0)}
  > User (Maybe Text)
@-}
userPhotoURL' :: EntityFieldWrapper User (Maybe Text)
userPhotoURL' = EntityFieldWrapper UserPhotoURL

{-@ measure userDisplayName :: User -> Text @-}

{-@ measure userDisplayNameCap :: Entity User -> Bool @-}

{-@ assume userDisplayName' :: EntityFieldWrapper <
    {\_ _ -> True}
  , {\row field -> field == userDisplayName (entityVal row)}
  , {\field row -> field == userDisplayName (entityVal row)}
  , {\old -> userDisplayNameCap old}
  , {\x_0 x_1 x_2 -> ((IsSelf x_0 x_2)) => (userDisplayNameCap x_0)}
  > User Text
@-}
userDisplayName' :: EntityFieldWrapper User Text
userDisplayName' = EntityFieldWrapper UserDisplayName

{-@ measure userInstitution :: User -> Text @-}

{-@ measure userInstitutionCap :: Entity User -> Bool @-}

{-@ assume userInstitution' :: EntityFieldWrapper <
    {\_ _ -> True}
  , {\row field -> field == userInstitution (entityVal row)}
  , {\field row -> field == userInstitution (entityVal row)}
  , {\old -> userInstitutionCap old}
  , {\x_0 x_1 x_2 -> ((IsSelf x_0 x_2)) => (userInstitutionCap x_0)}
  > User Text
@-}
userInstitution' :: EntityFieldWrapper User Text
userInstitution' = EntityFieldWrapper UserInstitution

{-@ measure userPronouns :: User -> Text @-}

{-@ measure userPronounsCap :: Entity User -> Bool @-}

{-@ assume userPronouns' :: EntityFieldWrapper <
    {\_ _ -> True}
  , {\row field -> field == userPronouns (entityVal row)}
  , {\field row -> field == userPronouns (entityVal row)}
  , {\old -> userPronounsCap old}
  , {\old _ _ -> userPronounsCap old}
  > User Text
@-}
userPronouns' :: EntityFieldWrapper User Text
userPronouns' = EntityFieldWrapper UserPronouns

{-@ measure userWebsite :: User -> Text @-}

{-@ measure userWebsiteCap :: Entity User -> Bool @-}

{-@ assume userWebsite' :: EntityFieldWrapper <
    {\_ _ -> True}
  , {\row field -> field == userWebsite (entityVal row)}
  , {\field row -> field == userWebsite (entityVal row)}
  , {\old -> userWebsiteCap old}
  , {\old _ _ -> userWebsiteCap old}
  > User Text
@-}
userWebsite' :: EntityFieldWrapper User Text
userWebsite' = EntityFieldWrapper UserWebsite

{-@ measure userBio :: User -> Text @-}

{-@ measure userBioCap :: Entity User -> Bool @-}

{-@ assume userBio' :: EntityFieldWrapper <
    {\_ _ -> True}
  , {\row field -> field == userBio (entityVal row)}
  , {\field row -> field == userBio (entityVal row)}
  , {\old -> userBioCap old}
  , {\old _ _ -> userBioCap old}
  > User Text
@-}
userBio' :: EntityFieldWrapper User Text
userBio' = EntityFieldWrapper UserBio

{-@ measure userLevel :: User -> String @-}

{-@ measure userLevelCap :: Entity User -> Bool @-}

{-@ assume userLevel' :: EntityFieldWrapper <
    {\_ _ -> True}
  , {\row field -> field == userLevel (entityVal row)}
  , {\field row -> field == userLevel (entityVal row)}
  , {\old -> userLevelCap old}
  , {\x_0 x_1 x_2 -> ((False)) => (userLevelCap x_0)}
  > User String
@-}
userLevel' :: EntityFieldWrapper User String
userLevel' = EntityFieldWrapper UserLevel

{-@ measure userVisibility :: User -> String @-}

{-@ measure userVisibilityCap :: Entity User -> Bool @-}

{-@ assume userVisibility' :: EntityFieldWrapper <
    {\_ _ -> True}
  , {\row field -> field == userVisibility (entityVal row)}
  , {\field row -> field == userVisibility (entityVal row)}
  , {\old -> userVisibilityCap old}
  , {\old _ _ -> userVisibilityCap old}
  > User String
@-}
userVisibility' :: EntityFieldWrapper User String
userVisibility' = EntityFieldWrapper UserVisibility

{-@ measure userRoom :: User -> (Maybe RoomId) @-}

{-@ measure userRoomCap :: Entity User -> Bool @-}

{-@ assume userRoom' :: EntityFieldWrapper <
    {\x_0 x_1 -> (userVisibility (entityVal x_0) == "public" || IsSelf x_0 x_1)}
  , {\row field -> field == userRoom (entityVal row)}
  , {\field row -> field == userRoom (entityVal row)}
  , {\old -> userRoomCap old}
  , {\x_0 x_1 x_2 -> ((IsSelf x_0 x_2)) => (userRoomCap x_0)}
  > User (Maybe RoomId)
@-}
userRoom' :: EntityFieldWrapper User (Maybe RoomId)
userRoom' = EntityFieldWrapper UserRoom

-- * Room
{-@ mkRoom ::
     x_0: Text
  -> x_1: Text
  -> x_2: Text
  -> x_3: Text
  -> BinahRecord <
       {\row -> roomColor (entityVal row) == x_0 && roomName (entityVal row) == x_1 && roomTopic (entityVal row) == x_2 && roomZoomLink (entityVal row) == x_3}
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
  > Room RoomId
@-}
roomId' :: EntityFieldWrapper Room RoomId
roomId' = EntityFieldWrapper RoomId

{-@ measure roomColor :: Room -> Text @-}

{-@ measure roomColorCap :: Entity Room -> Bool @-}

{-@ assume roomColor' :: EntityFieldWrapper <
    {\_ _ -> True}
  , {\row field -> field == roomColor (entityVal row)}
  , {\field row -> field == roomColor (entityVal row)}
  , {\old -> roomColorCap old}
  , {\old _ _ -> roomColorCap old}
  > Room Text
@-}
roomColor' :: EntityFieldWrapper Room Text
roomColor' = EntityFieldWrapper RoomColor

{-@ measure roomName :: Room -> Text @-}

{-@ measure roomNameCap :: Entity Room -> Bool @-}

{-@ assume roomName' :: EntityFieldWrapper <
    {\_ _ -> True}
  , {\row field -> field == roomName (entityVal row)}
  , {\field row -> field == roomName (entityVal row)}
  , {\old -> roomNameCap old}
  , {\x_0 x_1 x_2 -> ((IsOrganizer x_2)) => (roomNameCap x_0)}
  > Room Text
@-}
roomName' :: EntityFieldWrapper Room Text
roomName' = EntityFieldWrapper RoomName

{-@ measure roomTopic :: Room -> Text @-}

{-@ measure roomTopicCap :: Entity Room -> Bool @-}

{-@ assume roomTopic' :: EntityFieldWrapper <
    {\_ _ -> True}
  , {\row field -> field == roomTopic (entityVal row)}
  , {\field row -> field == roomTopic (entityVal row)}
  , {\old -> roomTopicCap old}
  , {\x_0 x_1 x_2 -> ((IsInRoom x_2 x_0) || (IsOrganizer x_2)) => (roomTopicCap x_0)}
  > Room Text
@-}
roomTopic' :: EntityFieldWrapper Room Text
roomTopic' = EntityFieldWrapper RoomTopic

{-@ measure roomZoomLink :: Room -> Text @-}

{-@ measure roomZoomLinkCap :: Entity Room -> Bool @-}

{-@ assume roomZoomLink' :: EntityFieldWrapper <
    {\_ _ -> True}
  , {\row field -> field == roomZoomLink (entityVal row)}
  , {\field row -> field == roomZoomLink (entityVal row)}
  , {\old -> roomZoomLinkCap old}
  , {\x_0 x_1 x_2 -> ((IsOrganizer x_2)) => (roomZoomLinkCap x_0)}
  > Room Text
@-}
roomZoomLink' :: EntityFieldWrapper Room Text
roomZoomLink' = EntityFieldWrapper RoomZoomLink

-- * Message
{-@ mkMessage ::
     x_0: UserId
  -> x_1: (Maybe UserId)
  -> x_2: Text
  -> x_3: Int64
  -> BinahRecord <
       {\row -> messageSender (entityVal row) == x_0 && messageReceiver (entityVal row) == x_1 && messageMessage (entityVal row) == x_2 && messageTimestamp (entityVal row) == x_3}
     , {\_ _ -> True}
     , {\x_0 x_1 -> False}
     > Message
@-}
mkMessage x_0 x_1 x_2 x_3 = BinahRecord (Message x_0 x_1 x_2 x_3)

{-@ invariant {v: Entity Message | v == getJust (entityKey v)} @-}



{-@ assume messageId' :: EntityFieldWrapper <
    {\row viewer -> True}
  , {\row field  -> field == entityKey row}
  , {\field row  -> field == entityKey row}
  , {\_ -> False}
  , {\_ _ _ -> True}
  > Message MessageId
@-}
messageId' :: EntityFieldWrapper Message MessageId
messageId' = EntityFieldWrapper MessageId

{-@ measure messageSender :: Message -> UserId @-}

{-@ measure messageSenderCap :: Entity Message -> Bool @-}

{-@ assume messageSender' :: EntityFieldWrapper <
    {\_ _ -> True}
  , {\row field -> field == messageSender (entityVal row)}
  , {\field row -> field == messageSender (entityVal row)}
  , {\old -> messageSenderCap old}
  , {\old _ _ -> messageSenderCap old}
  > Message UserId
@-}
messageSender' :: EntityFieldWrapper Message UserId
messageSender' = EntityFieldWrapper MessageSender

{-@ measure messageReceiver :: Message -> (Maybe UserId) @-}

{-@ measure messageReceiverCap :: Entity Message -> Bool @-}

{-@ assume messageReceiver' :: EntityFieldWrapper <
    {\_ _ -> True}
  , {\row field -> field == messageReceiver (entityVal row)}
  , {\field row -> field == messageReceiver (entityVal row)}
  , {\old -> messageReceiverCap old}
  , {\old _ _ -> messageReceiverCap old}
  > Message (Maybe UserId)
@-}
messageReceiver' :: EntityFieldWrapper Message (Maybe UserId)
messageReceiver' = EntityFieldWrapper MessageReceiver

{-@ measure messageMessage :: Message -> Text @-}

{-@ measure messageMessageCap :: Entity Message -> Bool @-}

{-@ assume messageMessage' :: EntityFieldWrapper <
    {\_ _ -> True}
  , {\row field -> field == messageMessage (entityVal row)}
  , {\field row -> field == messageMessage (entityVal row)}
  , {\old -> messageMessageCap old}
  , {\old _ _ -> messageMessageCap old}
  > Message Text
@-}
messageMessage' :: EntityFieldWrapper Message Text
messageMessage' = EntityFieldWrapper MessageMessage

{-@ measure messageTimestamp :: Message -> Int64 @-}

{-@ measure messageTimestampCap :: Entity Message -> Bool @-}

{-@ assume messageTimestamp' :: EntityFieldWrapper <
    {\_ _ -> True}
  , {\row field -> field == messageTimestamp (entityVal row)}
  , {\field row -> field == messageTimestamp (entityVal row)}
  , {\old -> messageTimestampCap old}
  , {\old _ _ -> messageTimestampCap old}
  > Message Int64
@-}
messageTimestamp' :: EntityFieldWrapper Message Int64
messageTimestamp' = EntityFieldWrapper MessageTimestamp

-- * MarkRead
{-@ mkMarkRead ::
     x_0: UserId
  -> x_1: MessageId
  -> BinahRecord <
       {\row -> markReadUser (entityVal row) == x_0 && markReadUpto (entityVal row) == x_1}
     , {\_ _ -> True}
     , {\x_0 x_1 -> False}
     > MarkRead
@-}
mkMarkRead x_0 x_1 = BinahRecord (MarkRead x_0 x_1)

{-@ invariant {v: Entity MarkRead | v == getJust (entityKey v)} @-}



{-@ assume markReadId' :: EntityFieldWrapper <
    {\row viewer -> True}
  , {\row field  -> field == entityKey row}
  , {\field row  -> field == entityKey row}
  , {\_ -> False}
  , {\_ _ _ -> True}
  > MarkRead MarkReadId
@-}
markReadId' :: EntityFieldWrapper MarkRead MarkReadId
markReadId' = EntityFieldWrapper MarkReadId

{-@ measure markReadUser :: MarkRead -> UserId @-}

{-@ measure markReadUserCap :: Entity MarkRead -> Bool @-}

{-@ assume markReadUser' :: EntityFieldWrapper <
    {\_ _ -> True}
  , {\row field -> field == markReadUser (entityVal row)}
  , {\field row -> field == markReadUser (entityVal row)}
  , {\old -> markReadUserCap old}
  , {\old _ _ -> markReadUserCap old}
  > MarkRead UserId
@-}
markReadUser' :: EntityFieldWrapper MarkRead UserId
markReadUser' = EntityFieldWrapper MarkReadUser

{-@ measure markReadUpto :: MarkRead -> MessageId @-}

{-@ measure markReadUptoCap :: Entity MarkRead -> Bool @-}

{-@ assume markReadUpto' :: EntityFieldWrapper <
    {\_ _ -> True}
  , {\row field -> field == markReadUpto (entityVal row)}
  , {\field row -> field == markReadUpto (entityVal row)}
  , {\old -> markReadUptoCap old}
  , {\old _ _ -> markReadUptoCap old}
  > MarkRead MessageId
@-}
markReadUpto' :: EntityFieldWrapper MarkRead MessageId
markReadUpto' = EntityFieldWrapper MarkReadUpto

--------------------------------------------------------------------------------
-- | Inline
--------------------------------------------------------------------------------


