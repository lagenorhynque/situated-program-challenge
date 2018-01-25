{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Handler.Meetups where

import           Import

import           Database.Esqueleto   ((^.))
import qualified Database.Esqueleto   as E

import           Handler.Members      (memberValueWithId)
import           Handler.OnlineVenues (onlineVenueValueWithId)
import           Handler.Venues       (venueValueWithAddress)

getMeetupsR :: GroupId -> Handler Value
getMeetupsR groupId = do
    ms <- runDB $ selectList [MeetupGroupId ==. Just groupId] []
    meetupDetails <- mapM fetchMeetupDetail ms
    returnJson meetupDetails

postMeetupsR :: GroupId -> Handler Value
postMeetupsR groupId = do
    m <- requireJsonBody :: Handler Meetup
    let m' = m { meetupGroupId = Just groupId }
    mid <- runDB $ insert m'
    meetupDetail <- fetchMeetupDetail $ Entity mid m'
    returnJson meetupDetail

getMeetupR :: GroupId -> MeetupId -> Handler Value
getMeetupR _ = getMeetup

postMeetupMemberR :: MemberId -> MeetupId -> Handler Value
postMeetupMemberR memberId meetupId = do
    _ <- runDB . insert $ MeetupMember { meetupMemberMeetupId = meetupId
                                       , meetupMemberMemberId = memberId
                                       }
    getMeetup meetupId

fetchMeetupDetail :: Entity Meetup -> Handler Value
fetchMeetupDetail em@(Entity mid Meetup{..}) = do
    v <- maybe (return Nothing) (runDB . getEntity) meetupVenueId
    ov <- maybe (return Nothing) (runDB . getEntity) meetupOnlineVenueId
    ms <- runDB $ E.select
                $ E.from $ \(member' `E.InnerJoin` meetupMember') -> do
                    E.on $ member' ^. MemberId E.==. meetupMember' ^. MeetupMemberMemberId
                    E.where_ $ meetupMember' ^. MeetupMemberMeetupId E.==. E.val mid
                    return member'
    return $ meetupWithVenueAndMembers em v ov ms

getMeetup :: MeetupId -> Handler Value
getMeetup mid = do
    m <- runDB $ get404 mid
    meetupDetail <- fetchMeetupDetail $ Entity mid m
    returnJson meetupDetail

meetupWithVenueAndMembers :: Entity Meetup -> Maybe (Entity Venue) -> Maybe (Entity Venue) -> [Entity Member] -> Value
meetupWithVenueAndMembers (Entity mid Meetup{..}) venue onlineVenue members =
    object [ "event-id" .= mid
           , "title" .= meetupTitle
           , "start-at" .= meetupStartAt
           , "end-at" .= meetupEndAt
           , "venue" .= fmap venueValueWithAddress venue
           , "online-venue" .= fmap onlineVenueValueWithId onlineVenue
           , "members" .= fmap memberValueWithId members
           ]
