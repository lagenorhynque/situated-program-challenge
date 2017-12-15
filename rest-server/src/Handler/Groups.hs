{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
module Handler.Groups where

import           Import

import           Data.Aeson.TH

import           Control.Lens       hiding ((.=), (^.))
import qualified Data.Aeson.Casing  as Casing
import           Data.Aeson.Lens
import           Database.Esqueleto ((^.))
import qualified Database.Esqueleto as E
import qualified Text.Casing        as Casing

import           Handler.Meetups    (fetchMeetupDetail)
import           Handler.Members    (memberValueWithId)
import           Handler.Venues     (venueValueWithAddress)

getGroupsR :: Handler Value
getGroupsR = do
    gs <- runDB $ selectList [] [] :: Handler [Entity Group]
    groupDetails <- mapM fetchGroupDetail gs
    returnJson groupDetails

postGroupsR :: Handler Value
postGroupsR = do
    PostGroup{..} <- requireJsonBody :: Handler PostGroup
    now <- liftIO getCurrentTime
    let g = Group { groupName = postGroupName
                  , groupCreatedAt = Just now
                  }
    gid <- runDB $ insert g
    _ <- runDB . insertMany $ map (\mid -> GroupMember { groupMemberGroupId = gid
                                                       , groupMemberMemberId = mid
                                                       , groupMemberAdmin = Just True
                                                       }) postAdminMemberIds
    ms <- runDB $ selectList [MemberId <-. postAdminMemberIds] []
    returnJson $ groupValueWithAdmin (Entity gid g) ms

postGroupMemberR :: MemberId -> GroupId -> Handler Value
postGroupMemberR memberId groupId = do
    PostGroupMember{..} <- requireJsonBody :: Handler PostGroupMember
    _ <- runDB $ insert GroupMember { groupMemberGroupId = groupId
                                    , groupMemberMemberId = memberId
                                    , groupMemberAdmin = postAdmin
                                    }
    g <- runDB $ get404 groupId
    groupDetail <- fetchGroupDetail $ Entity groupId g
    returnJson groupDetail

fetchGroupDetail :: Entity Group -> Handler Value
fetchGroupDetail eg@(Entity gid Group{..}) = do
    admins <- runDB $ E.select
                    $ E.from $ \(member' `E.InnerJoin` groupMember') -> do
                        E.on $ member' ^. MemberId E.==. groupMember' ^. GroupMemberMemberId
                        E.where_ $ (groupMember' ^. GroupMemberGroupId E.==. E.val gid)
                            E.&&. (groupMember' ^. GroupMemberAdmin E.==. E.val (Just True))
                        return member'
    vs <- runDB $ selectList [VenueGroupId ==. Just gid] []
    let venueDetails = map venueValueWithAddress vs
    ms <- runDB $ selectList [MeetupGroupId ==. Just gid] []
    meetupDetails <- mapM fetchMeetupDetail ms
    return $ toJSON (groupValueWithAdmin eg admins)
             & _Object . at "venues" ?~ toJSON venueDetails
             & _Object . at "meetups" ?~ toJSON meetupDetails

groupValueWithAdmin :: Entity Group -> [Entity Member] -> Value
groupValueWithAdmin (Entity gid Group{..}) ms =
    object [ "group-id" .= gid
           , "group-name" .= groupName
           , "admin" .= map memberValueWithId ms
           ]

data PostGroup = PostGroup
    { postGroupName      :: Maybe Text
    , postAdminMemberIds :: [Key Member]
    }

newtype PostGroupMember = PostGroupMember
    { postAdmin :: Maybe Bool
    }

deriveJSON (Casing.aesonPrefix Casing.kebab) ''PostGroup
deriveJSON (Casing.aesonPrefix Casing.kebab) ''PostGroupMember
