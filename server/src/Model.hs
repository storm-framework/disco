{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

{-@ LIQUID "--compile-spec" @-}

module Model
  ( migrateAll
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
  , userActive'
  , userLastSync'
  , roomId'
  , roomColor'
  , roomName'
  , roomTopic'
  , roomCapacity'
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
import qualified Database.Persist              as Persist

import           Storm.Core

import Data.ByteString (ByteString)
import Data.Int        (Int64)
import Data.Time.Clock (UTCTime)
import Data.Text       (Text)

--------------------------------------------------------------------------------
-- | Inline
--------------------------------------------------------------------------------



--------------------------------------------------------------------------------
-- | Persistent
--------------------------------------------------------------------------------

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
  active Bool
  lastSync UTCTime
  UniqueUserEmailAddress emailAddress

Room
  color Text
  name Text
  topic Text
  capacity Int
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

--------------------------------------------------------------------------------
-- | Predicates
--------------------------------------------------------------------------------



--------------------------------------------------------------------------------
-- | Policies
--------------------------------------------------------------------------------

{-@ predicate CanReadMessage MESSAGE VIEWER = fromJust (messageReceiver (entityVal MESSAGE)) == entityKey VIEWER || not (isJust (messageReceiver (entityVal MESSAGE))) @-}

{-@ predicate IsInRoom VIEWER ROOM = fromJust (userRoom (entityVal VIEWER)) == entityKey ROOM @-}

{-@ predicate IsSelf USER VIEWER = USER == VIEWER @-}

{-@ predicate IsOrganizer USER = userLevel (entityVal USER) == "organizer" @-}

--------------------------------------------------------------------------------
-- | Records
--------------------------------------------------------------------------------

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
     -> x_7: Maybe String
     -> StormRecord <{\row -> invitationCode (entityVal row) == x_0 && invitationEmailAddress (entityVal row) == x_1 && invitationFirstName (entityVal row) == x_2 && invitationLastName (entityVal row) == x_3 && invitationInstitution (entityVal row) == x_4 && invitationAccepted (entityVal row) == x_5 && invitationEmailStatus (entityVal row) == x_6 && invitationEmailError (entityVal row) == x_7},
                     {\invitation viewer -> not (invitationAccepted (entityVal invitation)) && IsOrganizer viewer && invitationEmailStatus (entityVal invitation) == "not_sent"},
                     {\x_0 x_1 -> False}>
                     (Entity User) Invitation
  @-}
mkInvitation :: Text -> Text -> Text -> Text -> Text -> Bool -> String -> Maybe String -> StormRecord (Entity User) Invitation
mkInvitation x_0 x_1 x_2 x_3 x_4 x_5 x_6 x_7 = StormRecord (Invitation x_0 x_1 x_2 x_3 x_4 x_5 x_6 x_7)

{-@ invariant {v: Entity Invitation | v == getJust (entityKey v)} @-}



{-@ assume invitationId' ::
      EntityFieldWrapper <{\row viewer -> True},
                          {\row field  -> field == entityKey row},
                          {\field row  -> field == entityKey row},
                          {\_ -> False},
                          {\_ _ _ -> True}>
                          (Entity User) Invitation InvitationId
  @-}
invitationId' :: EntityFieldWrapper (Entity User) Invitation InvitationId
invitationId' = EntityFieldWrapper InvitationId

{-@ measure invitationCode :: Invitation -> Text @-}

{-@ measure invitationCodeCap :: Entity Invitation -> Bool @-}

{-@ assume invitationCode' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == invitationCode (entityVal row)},
                          {\field row -> field == invitationCode (entityVal row)},
                          {\old -> invitationCodeCap old},
                          {\old _ _ -> invitationCodeCap old}>
                          (Entity User) Invitation Text
  @-}
invitationCode' :: EntityFieldWrapper (Entity User) Invitation Text
invitationCode' = EntityFieldWrapper InvitationCode

{-@ measure invitationEmailAddress :: Invitation -> Text @-}

{-@ measure invitationEmailAddressCap :: Entity Invitation -> Bool @-}

{-@ assume invitationEmailAddress' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == invitationEmailAddress (entityVal row)},
                          {\field row -> field == invitationEmailAddress (entityVal row)},
                          {\old -> invitationEmailAddressCap old},
                          {\old _ _ -> invitationEmailAddressCap old}>
                          (Entity User) Invitation Text
  @-}
invitationEmailAddress' :: EntityFieldWrapper (Entity User) Invitation Text
invitationEmailAddress' = EntityFieldWrapper InvitationEmailAddress

{-@ measure invitationFirstName :: Invitation -> Text @-}

{-@ measure invitationFirstNameCap :: Entity Invitation -> Bool @-}

{-@ assume invitationFirstName' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == invitationFirstName (entityVal row)},
                          {\field row -> field == invitationFirstName (entityVal row)},
                          {\old -> invitationFirstNameCap old},
                          {\old _ _ -> invitationFirstNameCap old}>
                          (Entity User) Invitation Text
  @-}
invitationFirstName' :: EntityFieldWrapper (Entity User) Invitation Text
invitationFirstName' = EntityFieldWrapper InvitationFirstName

{-@ measure invitationLastName :: Invitation -> Text @-}

{-@ measure invitationLastNameCap :: Entity Invitation -> Bool @-}

{-@ assume invitationLastName' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == invitationLastName (entityVal row)},
                          {\field row -> field == invitationLastName (entityVal row)},
                          {\old -> invitationLastNameCap old},
                          {\old _ _ -> invitationLastNameCap old}>
                          (Entity User) Invitation Text
  @-}
invitationLastName' :: EntityFieldWrapper (Entity User) Invitation Text
invitationLastName' = EntityFieldWrapper InvitationLastName

{-@ measure invitationInstitution :: Invitation -> Text @-}

{-@ measure invitationInstitutionCap :: Entity Invitation -> Bool @-}

{-@ assume invitationInstitution' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == invitationInstitution (entityVal row)},
                          {\field row -> field == invitationInstitution (entityVal row)},
                          {\old -> invitationInstitutionCap old},
                          {\old _ _ -> invitationInstitutionCap old}>
                          (Entity User) Invitation Text
  @-}
invitationInstitution' :: EntityFieldWrapper (Entity User) Invitation Text
invitationInstitution' = EntityFieldWrapper InvitationInstitution

{-@ measure invitationAccepted :: Invitation -> Bool @-}

{-@ measure invitationAcceptedCap :: Entity Invitation -> Bool @-}

{-@ assume invitationAccepted' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == invitationAccepted (entityVal row)},
                          {\field row -> field == invitationAccepted (entityVal row)},
                          {\old -> invitationAcceptedCap old},
                          {\x_0 x_1 x_2 -> ((not (invitationAccepted (entityVal x_0)) && invitationAccepted (entityVal x_1))) => (invitationAcceptedCap x_0)}>
                          (Entity User) Invitation Bool
  @-}
invitationAccepted' :: EntityFieldWrapper (Entity User) Invitation Bool
invitationAccepted' = EntityFieldWrapper InvitationAccepted

{-@ measure invitationEmailStatus :: Invitation -> String @-}

{-@ measure invitationEmailStatusCap :: Entity Invitation -> Bool @-}

{-@ assume invitationEmailStatus' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == invitationEmailStatus (entityVal row)},
                          {\field row -> field == invitationEmailStatus (entityVal row)},
                          {\old -> invitationEmailStatusCap old},
                          {\x_0 x_1 x_2 -> (((invitationEmailStatus (entityVal x_1) == "sent" || invitationEmailStatus (entityVal x_1) == "error"))) => (invitationEmailStatusCap x_0)}>
                          (Entity User) Invitation String
  @-}
invitationEmailStatus' :: EntityFieldWrapper (Entity User) Invitation String
invitationEmailStatus' = EntityFieldWrapper InvitationEmailStatus

{-@ measure invitationEmailError :: Invitation -> (Maybe String) @-}

{-@ measure invitationEmailErrorCap :: Entity Invitation -> Bool @-}

{-@ assume invitationEmailError' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == invitationEmailError (entityVal row)},
                          {\field row -> field == invitationEmailError (entityVal row)},
                          {\old -> invitationEmailErrorCap old},
                          {\old _ _ -> invitationEmailErrorCap old}>
                          (Entity User) Invitation (Maybe String)
  @-}
invitationEmailError' :: EntityFieldWrapper (Entity User) Invitation (Maybe String)
invitationEmailError' = EntityFieldWrapper InvitationEmailError

-- * User
{-@ mkUser ::
        x_0: Text
     -> x_1: ByteString
     -> x_2: Maybe Text
     -> x_3: Text
     -> x_4: Text
     -> x_5: Text
     -> x_6: Text
     -> x_7: Text
     -> x_8: String
     -> x_9: String
     -> x_10: Maybe RoomId
     -> x_11: Bool
     -> x_12: UTCTime
     -> StormRecord <{\row -> userEmailAddress (entityVal row) == x_0 && userPassword (entityVal row) == x_1 && userPhotoURL (entityVal row) == x_2 && userDisplayName (entityVal row) == x_3 && userInstitution (entityVal row) == x_4 && userPronouns (entityVal row) == x_5 && userWebsite (entityVal row) == x_6 && userBio (entityVal row) == x_7 && userLevel (entityVal row) == x_8 && userVisibility (entityVal row) == x_9 && userRoom (entityVal row) == x_10 && userActive (entityVal row) == x_11 && userLastSync (entityVal row) == x_12},
                     {\new viewer -> IsOrganizer viewer || userLevel (entityVal new) == "attendee"},
                     {\x_0 x_1 -> (userVisibility (entityVal x_0) == "public" || IsSelf x_0 x_1) || (x_0 == x_1)}>
                     (Entity User) User
  @-}
mkUser :: Text -> ByteString -> Maybe Text -> Text -> Text -> Text -> Text -> Text -> String -> String -> Maybe RoomId -> Bool -> UTCTime -> StormRecord (Entity User) User
mkUser x_0 x_1 x_2 x_3 x_4 x_5 x_6 x_7 x_8 x_9 x_10 x_11 x_12 = StormRecord (User x_0 x_1 x_2 x_3 x_4 x_5 x_6 x_7 x_8 x_9 x_10 x_11 x_12)

{-@ invariant {v: Entity User | v == getJust (entityKey v)} @-}



{-@ assume userId' ::
      EntityFieldWrapper <{\row viewer -> True},
                          {\row field  -> field == entityKey row},
                          {\field row  -> field == entityKey row},
                          {\_ -> False},
                          {\_ _ _ -> True}>
                          (Entity User) User UserId
  @-}
userId' :: EntityFieldWrapper (Entity User) User UserId
userId' = EntityFieldWrapper UserId

{-@ measure userEmailAddress :: User -> Text @-}

{-@ measure userEmailAddressCap :: Entity User -> Bool @-}

{-@ assume userEmailAddress' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == userEmailAddress (entityVal row)},
                          {\field row -> field == userEmailAddress (entityVal row)},
                          {\old -> userEmailAddressCap old},
                          {\x_0 x_1 x_2 -> ((False)) => (userEmailAddressCap x_0)}>
                          (Entity User) User Text
  @-}
userEmailAddress' :: EntityFieldWrapper (Entity User) User Text
userEmailAddress' = EntityFieldWrapper UserEmailAddress

{-@ measure userPassword :: User -> ByteString @-}

{-@ measure userPasswordCap :: Entity User -> Bool @-}

{-@ assume userPassword' ::
      EntityFieldWrapper <{\x_0 x_1 -> (x_0 == x_1)},
                          {\row field -> field == userPassword (entityVal row)},
                          {\field row -> field == userPassword (entityVal row)},
                          {\old -> userPasswordCap old},
                          {\x_0 x_1 x_2 -> ((False)) => (userPasswordCap x_0)}>
                          (Entity User) User ByteString
  @-}
userPassword' :: EntityFieldWrapper (Entity User) User ByteString
userPassword' = EntityFieldWrapper UserPassword

{-@ measure userPhotoURL :: User -> (Maybe Text) @-}

{-@ measure userPhotoURLCap :: Entity User -> Bool @-}

{-@ assume userPhotoURL' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == userPhotoURL (entityVal row)},
                          {\field row -> field == userPhotoURL (entityVal row)},
                          {\old -> userPhotoURLCap old},
                          {\x_0 x_1 x_2 -> ((IsSelf x_0 x_2)) => (userPhotoURLCap x_0)}>
                          (Entity User) User (Maybe Text)
  @-}
userPhotoURL' :: EntityFieldWrapper (Entity User) User (Maybe Text)
userPhotoURL' = EntityFieldWrapper UserPhotoURL

{-@ measure userDisplayName :: User -> Text @-}

{-@ measure userDisplayNameCap :: Entity User -> Bool @-}

{-@ assume userDisplayName' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == userDisplayName (entityVal row)},
                          {\field row -> field == userDisplayName (entityVal row)},
                          {\old -> userDisplayNameCap old},
                          {\x_0 x_1 x_2 -> ((IsSelf x_0 x_2)) => (userDisplayNameCap x_0)}>
                          (Entity User) User Text
  @-}
userDisplayName' :: EntityFieldWrapper (Entity User) User Text
userDisplayName' = EntityFieldWrapper UserDisplayName

{-@ measure userInstitution :: User -> Text @-}

{-@ measure userInstitutionCap :: Entity User -> Bool @-}

{-@ assume userInstitution' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == userInstitution (entityVal row)},
                          {\field row -> field == userInstitution (entityVal row)},
                          {\old -> userInstitutionCap old},
                          {\x_0 x_1 x_2 -> ((IsSelf x_0 x_2)) => (userInstitutionCap x_0)}>
                          (Entity User) User Text
  @-}
userInstitution' :: EntityFieldWrapper (Entity User) User Text
userInstitution' = EntityFieldWrapper UserInstitution

{-@ measure userPronouns :: User -> Text @-}

{-@ measure userPronounsCap :: Entity User -> Bool @-}

{-@ assume userPronouns' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == userPronouns (entityVal row)},
                          {\field row -> field == userPronouns (entityVal row)},
                          {\old -> userPronounsCap old},
                          {\x_0 x_1 x_2 -> ((IsSelf x_0 x_2)) => (userPronounsCap x_0)}>
                          (Entity User) User Text
  @-}
userPronouns' :: EntityFieldWrapper (Entity User) User Text
userPronouns' = EntityFieldWrapper UserPronouns

{-@ measure userWebsite :: User -> Text @-}

{-@ measure userWebsiteCap :: Entity User -> Bool @-}

{-@ assume userWebsite' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == userWebsite (entityVal row)},
                          {\field row -> field == userWebsite (entityVal row)},
                          {\old -> userWebsiteCap old},
                          {\x_0 x_1 x_2 -> ((IsSelf x_0 x_2)) => (userWebsiteCap x_0)}>
                          (Entity User) User Text
  @-}
userWebsite' :: EntityFieldWrapper (Entity User) User Text
userWebsite' = EntityFieldWrapper UserWebsite

{-@ measure userBio :: User -> Text @-}

{-@ measure userBioCap :: Entity User -> Bool @-}

{-@ assume userBio' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == userBio (entityVal row)},
                          {\field row -> field == userBio (entityVal row)},
                          {\old -> userBioCap old},
                          {\x_0 x_1 x_2 -> ((IsSelf x_0 x_2)) => (userBioCap x_0)}>
                          (Entity User) User Text
  @-}
userBio' :: EntityFieldWrapper (Entity User) User Text
userBio' = EntityFieldWrapper UserBio

{-@ measure userLevel :: User -> String @-}

{-@ measure userLevelCap :: Entity User -> Bool @-}

{-@ assume userLevel' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == userLevel (entityVal row)},
                          {\field row -> field == userLevel (entityVal row)},
                          {\old -> userLevelCap old},
                          {\x_0 x_1 x_2 -> ((False)) => (userLevelCap x_0)}>
                          (Entity User) User String
  @-}
userLevel' :: EntityFieldWrapper (Entity User) User String
userLevel' = EntityFieldWrapper UserLevel

{-@ measure userVisibility :: User -> String @-}

{-@ measure userVisibilityCap :: Entity User -> Bool @-}

{-@ assume userVisibility' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == userVisibility (entityVal row)},
                          {\field row -> field == userVisibility (entityVal row)},
                          {\old -> userVisibilityCap old},
                          {\x_0 x_1 x_2 -> ((IsSelf x_0 x_2)) => (userVisibilityCap x_0)}>
                          (Entity User) User String
  @-}
userVisibility' :: EntityFieldWrapper (Entity User) User String
userVisibility' = EntityFieldWrapper UserVisibility

{-@ measure userRoom :: User -> (Maybe RoomId) @-}

{-@ measure userRoomCap :: Entity User -> Bool @-}

{-@ assume userRoom' ::
      EntityFieldWrapper <{\x_0 x_1 -> (userVisibility (entityVal x_0) == "public" || IsSelf x_0 x_1)},
                          {\row field -> field == userRoom (entityVal row)},
                          {\field row -> field == userRoom (entityVal row)},
                          {\old -> userRoomCap old},
                          {\x_0 x_1 x_2 -> ((IsSelf x_0 x_2)) => (userRoomCap x_0)}>
                          (Entity User) User (Maybe RoomId)
  @-}
userRoom' :: EntityFieldWrapper (Entity User) User (Maybe RoomId)
userRoom' = EntityFieldWrapper UserRoom

{-@ measure userActive :: User -> Bool @-}

{-@ measure userActiveCap :: Entity User -> Bool @-}

{-@ assume userActive' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == userActive (entityVal row)},
                          {\field row -> field == userActive (entityVal row)},
                          {\old -> userActiveCap old},
                          {\x_0 x_1 x_2 -> ((IsSelf x_0 x_2)) => (userActiveCap x_0)}>
                          (Entity User) User Bool
  @-}
userActive' :: EntityFieldWrapper (Entity User) User Bool
userActive' = EntityFieldWrapper UserActive

{-@ measure userLastSync :: User -> UTCTime @-}

{-@ measure userLastSyncCap :: Entity User -> Bool @-}

{-@ assume userLastSync' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == userLastSync (entityVal row)},
                          {\field row -> field == userLastSync (entityVal row)},
                          {\old -> userLastSyncCap old},
                          {\x_0 x_1 x_2 -> ((IsSelf x_0 x_2)) => (userLastSyncCap x_0)}>
                          (Entity User) User UTCTime
  @-}
userLastSync' :: EntityFieldWrapper (Entity User) User UTCTime
userLastSync' = EntityFieldWrapper UserLastSync

-- * Room
{-@ mkRoom ::
        x_0: Text
     -> x_1: Text
     -> x_2: Text
     -> x_3: Int
     -> x_4: Text
     -> StormRecord <{\row -> roomColor (entityVal row) == x_0 && roomName (entityVal row) == x_1 && roomTopic (entityVal row) == x_2 && roomCapacity (entityVal row) == x_3 && roomZoomLink (entityVal row) == x_4},
                     {\_ viewer -> IsOrganizer viewer},
                     {\x_0 x_1 -> False}>
                     (Entity User) Room
  @-}
mkRoom :: Text -> Text -> Text -> Int -> Text -> StormRecord (Entity User) Room
mkRoom x_0 x_1 x_2 x_3 x_4 = StormRecord (Room x_0 x_1 x_2 x_3 x_4)

{-@ invariant {v: Entity Room | v == getJust (entityKey v)} @-}



{-@ assume roomId' ::
      EntityFieldWrapper <{\row viewer -> True},
                          {\row field  -> field == entityKey row},
                          {\field row  -> field == entityKey row},
                          {\_ -> False},
                          {\_ _ _ -> True}>
                          (Entity User) Room RoomId
  @-}
roomId' :: EntityFieldWrapper (Entity User) Room RoomId
roomId' = EntityFieldWrapper RoomId

{-@ measure roomColor :: Room -> Text @-}

{-@ measure roomColorCap :: Entity Room -> Bool @-}

{-@ assume roomColor' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == roomColor (entityVal row)},
                          {\field row -> field == roomColor (entityVal row)},
                          {\old -> roomColorCap old},
                          {\old _ _ -> roomColorCap old}>
                          (Entity User) Room Text
  @-}
roomColor' :: EntityFieldWrapper (Entity User) Room Text
roomColor' = EntityFieldWrapper RoomColor

{-@ measure roomName :: Room -> Text @-}

{-@ measure roomNameCap :: Entity Room -> Bool @-}

{-@ assume roomName' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == roomName (entityVal row)},
                          {\field row -> field == roomName (entityVal row)},
                          {\old -> roomNameCap old},
                          {\x_0 x_1 x_2 -> ((IsOrganizer x_2)) => (roomNameCap x_0)}>
                          (Entity User) Room Text
  @-}
roomName' :: EntityFieldWrapper (Entity User) Room Text
roomName' = EntityFieldWrapper RoomName

{-@ measure roomTopic :: Room -> Text @-}

{-@ measure roomTopicCap :: Entity Room -> Bool @-}

{-@ assume roomTopic' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == roomTopic (entityVal row)},
                          {\field row -> field == roomTopic (entityVal row)},
                          {\old -> roomTopicCap old},
                          {\x_0 x_1 x_2 -> ((IsInRoom x_2 x_0) || (IsOrganizer x_2)) => (roomTopicCap x_0)}>
                          (Entity User) Room Text
  @-}
roomTopic' :: EntityFieldWrapper (Entity User) Room Text
roomTopic' = EntityFieldWrapper RoomTopic

{-@ measure roomCapacity :: Room -> Int @-}

{-@ measure roomCapacityCap :: Entity Room -> Bool @-}

{-@ assume roomCapacity' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == roomCapacity (entityVal row)},
                          {\field row -> field == roomCapacity (entityVal row)},
                          {\old -> roomCapacityCap old},
                          {\x_0 x_1 x_2 -> ((IsOrganizer x_2)) => (roomCapacityCap x_0)}>
                          (Entity User) Room Int
  @-}
roomCapacity' :: EntityFieldWrapper (Entity User) Room Int
roomCapacity' = EntityFieldWrapper RoomCapacity

{-@ measure roomZoomLink :: Room -> Text @-}

{-@ measure roomZoomLinkCap :: Entity Room -> Bool @-}

{-@ assume roomZoomLink' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == roomZoomLink (entityVal row)},
                          {\field row -> field == roomZoomLink (entityVal row)},
                          {\old -> roomZoomLinkCap old},
                          {\x_0 x_1 x_2 -> ((IsOrganizer x_2)) => (roomZoomLinkCap x_0)}>
                          (Entity User) Room Text
  @-}
roomZoomLink' :: EntityFieldWrapper (Entity User) Room Text
roomZoomLink' = EntityFieldWrapper RoomZoomLink

-- * Message
{-@ mkMessage ::
        x_0: UserId
     -> x_1: Maybe UserId
     -> x_2: Text
     -> x_3: Int64
     -> StormRecord <{\row -> messageSender (entityVal row) == x_0 && messageReceiver (entityVal row) == x_1 && messageMessage (entityVal row) == x_2 && messageTimestamp (entityVal row) == x_3},
                     {\_ _ -> True},
                     {\x_0 x_1 -> (fromJust (messageReceiver (entityVal x_0)) == entityKey x_1 || not (isJust (messageReceiver (entityVal x_0))))}>
                     (Entity User) Message
  @-}
mkMessage :: UserId -> Maybe UserId -> Text -> Int64 -> StormRecord (Entity User) Message
mkMessage x_0 x_1 x_2 x_3 = StormRecord (Message x_0 x_1 x_2 x_3)

{-@ invariant {v: Entity Message | v == getJust (entityKey v)} @-}



{-@ assume messageId' ::
      EntityFieldWrapper <{\row viewer -> True},
                          {\row field  -> field == entityKey row},
                          {\field row  -> field == entityKey row},
                          {\_ -> False},
                          {\_ _ _ -> True}>
                          (Entity User) Message MessageId
  @-}
messageId' :: EntityFieldWrapper (Entity User) Message MessageId
messageId' = EntityFieldWrapper MessageId

{-@ measure messageSender :: Message -> UserId @-}

{-@ measure messageSenderCap :: Entity Message -> Bool @-}

{-@ assume messageSender' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == messageSender (entityVal row)},
                          {\field row -> field == messageSender (entityVal row)},
                          {\old -> messageSenderCap old},
                          {\old _ _ -> messageSenderCap old}>
                          (Entity User) Message UserId
  @-}
messageSender' :: EntityFieldWrapper (Entity User) Message UserId
messageSender' = EntityFieldWrapper MessageSender

{-@ measure messageReceiver :: Message -> (Maybe UserId) @-}

{-@ measure messageReceiverCap :: Entity Message -> Bool @-}

{-@ assume messageReceiver' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == messageReceiver (entityVal row)},
                          {\field row -> field == messageReceiver (entityVal row)},
                          {\old -> messageReceiverCap old},
                          {\old _ _ -> messageReceiverCap old}>
                          (Entity User) Message (Maybe UserId)
  @-}
messageReceiver' :: EntityFieldWrapper (Entity User) Message (Maybe UserId)
messageReceiver' = EntityFieldWrapper MessageReceiver

{-@ measure messageMessage :: Message -> Text @-}

{-@ measure messageMessageCap :: Entity Message -> Bool @-}

{-@ assume messageMessage' ::
      EntityFieldWrapper <{\x_0 x_1 -> (fromJust (messageReceiver (entityVal x_0)) == entityKey x_1 || not (isJust (messageReceiver (entityVal x_0))))},
                          {\row field -> field == messageMessage (entityVal row)},
                          {\field row -> field == messageMessage (entityVal row)},
                          {\old -> messageMessageCap old},
                          {\old _ _ -> messageMessageCap old}>
                          (Entity User) Message Text
  @-}
messageMessage' :: EntityFieldWrapper (Entity User) Message Text
messageMessage' = EntityFieldWrapper MessageMessage

{-@ measure messageTimestamp :: Message -> Int64 @-}

{-@ measure messageTimestampCap :: Entity Message -> Bool @-}

{-@ assume messageTimestamp' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == messageTimestamp (entityVal row)},
                          {\field row -> field == messageTimestamp (entityVal row)},
                          {\old -> messageTimestampCap old},
                          {\old _ _ -> messageTimestampCap old}>
                          (Entity User) Message Int64
  @-}
messageTimestamp' :: EntityFieldWrapper (Entity User) Message Int64
messageTimestamp' = EntityFieldWrapper MessageTimestamp

-- * MarkRead
{-@ mkMarkRead ::
        x_0: UserId
     -> x_1: MessageId
     -> StormRecord <{\row -> markReadUser (entityVal row) == x_0 && markReadUpto (entityVal row) == x_1},
                     {\_ _ -> True},
                     {\x_0 x_1 -> False}>
                     (Entity User) MarkRead
  @-}
mkMarkRead :: UserId -> MessageId -> StormRecord (Entity User) MarkRead
mkMarkRead x_0 x_1 = StormRecord (MarkRead x_0 x_1)

{-@ invariant {v: Entity MarkRead | v == getJust (entityKey v)} @-}



{-@ assume markReadId' ::
      EntityFieldWrapper <{\row viewer -> True},
                          {\row field  -> field == entityKey row},
                          {\field row  -> field == entityKey row},
                          {\_ -> False},
                          {\_ _ _ -> True}>
                          (Entity User) MarkRead MarkReadId
  @-}
markReadId' :: EntityFieldWrapper (Entity User) MarkRead MarkReadId
markReadId' = EntityFieldWrapper MarkReadId

{-@ measure markReadUser :: MarkRead -> UserId @-}

{-@ measure markReadUserCap :: Entity MarkRead -> Bool @-}

{-@ assume markReadUser' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == markReadUser (entityVal row)},
                          {\field row -> field == markReadUser (entityVal row)},
                          {\old -> markReadUserCap old},
                          {\old _ _ -> markReadUserCap old}>
                          (Entity User) MarkRead UserId
  @-}
markReadUser' :: EntityFieldWrapper (Entity User) MarkRead UserId
markReadUser' = EntityFieldWrapper MarkReadUser

{-@ measure markReadUpto :: MarkRead -> MessageId @-}

{-@ measure markReadUptoCap :: Entity MarkRead -> Bool @-}

{-@ assume markReadUpto' ::
      EntityFieldWrapper <{\_ _ -> True},
                          {\row field -> field == markReadUpto (entityVal row)},
                          {\field row -> field == markReadUpto (entityVal row)},
                          {\old -> markReadUptoCap old},
                          {\old _ _ -> markReadUptoCap old}>
                          (Entity User) MarkRead MessageId
  @-}
markReadUpto' :: EntityFieldWrapper (Entity User) MarkRead MessageId
markReadUpto' = EntityFieldWrapper MarkReadUpto
