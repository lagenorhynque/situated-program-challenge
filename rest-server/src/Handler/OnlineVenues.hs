{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
module Handler.OnlineVenues where

import           Import

import           Data.Aeson.TH

import qualified Data.Aeson.Casing     as Casing
import qualified Text.Casing           as Casing

import qualified CustomField.VenueType as VenueType

getOnlineVenuesR :: GroupId -> Handler Value
getOnlineVenuesR groupId = do
    vs <- runDB $ selectList [VenueGroupId ==. Just groupId, VenueVenueType ==. Just VenueType.Online] []
    returnJson $ map onlineVenueValueWithId vs

postOnlineVenuesR :: GroupId -> Handler Value
postOnlineVenuesR groupId = do
    PostVenue{..} <- requireJsonBody :: Handler PostVenue
    let v = Venue { venueName = postVenueName
                  , venuePostalCode = Nothing
                  , venuePrefecture = Nothing
                  , venueCity = Nothing
                  , venueStreet1 = Nothing
                  , venueStreet2 = Nothing
                  , venueGroupId = Just groupId
                  , venueUrl = postUrl
                  , venueVenueType = Just VenueType.Online
                  }
    vid <- runDB $ insert v
    returnJson . onlineVenueValueWithId $ Entity vid v

onlineVenueValueWithId :: Entity Venue -> Value
onlineVenueValueWithId (Entity vid Venue{..}) =
    object [ "online-venue-id" .= vid
           , "venue-name" .= venueName
           , "url" .= venueUrl
           ]

data PostVenue = PostVenue
    { postVenueName :: Maybe Text
    , postUrl       :: Maybe Text
    }

deriveJSON (Casing.aesonPrefix Casing.kebab) ''PostVenue
